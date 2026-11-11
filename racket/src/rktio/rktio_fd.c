#include "rktio.h"
#include "rktio_private.h"
#include <errno.h>
#include <stdlib.h>
#ifdef RKTIO_SYSTEM_UNIX
# include <sys/stat.h>
# include <fcntl.h>
# include <unistd.h>
# include <sys/select.h>
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
# include <windows.h>
# include <string.h>
#endif
#ifdef HAVE_POLL_SYSCALL
# include <poll.h>
#endif
#if defined(RKTIO_SYSTEM_UNIX) && defined(__linux__)
# include <sys/sendfile.h>
#endif

/*========================================================================*/
/* fd struct                                                              */
/*========================================================================*/

struct rktio_fd_t {
  int modes;

#ifdef RKTIO_SYSTEM_UNIX
  intptr_t fd;
# ifdef SOME_FDS_ARE_NOT_SELECTABLE
  int bufcount;
  char buffer[1];
# endif
# ifdef RKTIO_USE_PENDING_OPEN
  struct open_in_thread_t *pending;
# endif
# ifdef RKTIO_USE_IO_URING
  struct rktio_uring_fd_t *u; /* non-NULL => socket I/O via io_uring */
# endif
#endif
  
#ifdef RKTIO_SYSTEM_WINDOWS
  union {
    HANDLE fd;
    SOCKET sock; /* when `modes & RKTIO_OPEN_SOCKET` */
  };
  struct Win_FD_Input_Thread *th; /* input mode */
  struct Win_FD_Output_Thread *oth; /* output mode */
  int unblocked; /* whether non-blocking mode is installed */
  int write_limit; /* non-0 => max on amount to try writing */
  char *buffer; /* shared with reading thread */
  int has_pending_byte; /* for text-mode input, may be dropped by a following lf */
  int pending_byte; /* for text-mode input, either a CR waiting to decode, or byte that didn't fit */
  int leftover_len; /* for bytes that should be written/read, but that form a UTF-8 encoding prefix/suffix */
  char leftover[6];
  int w_buf_count; /* number of console wide chars in `buffer` pending to encode */
#endif

  rktio_result_t res;
};

struct rktio_fd_transfer_t {
  rktio_fd_t fd;
};

/*========================================================================*/
/* Experimental io_uring socket I/O                                       */
/*========================================================================*/

/* When the PLT_RKTIO_URING environment variable is set, connected TCP
   sockets do their reads and writes through an io_uring that is
   private to the `rktio_t` (i.e., per place). A read or write does not
   call into the kernel; it copies to or from a C-side buffer owned by
   the socket and queues an SQE. Queued SQEs are submitted all at once
   by `rktio_uring_submit_and_reap`, which the long-term poll set calls
   on each scheduler poll, and by `rktio_sleep` before sleeping, so all
   the I/O that Racket threads start in one scheduler round costs one
   `io_uring_enter`. Completions signal long-term poll set handles,
   just like epoll readiness does, and the ring's fd is added to the
   sleep poll set so that a sleep ends when an operation completes. */

#ifdef RKTIO_USE_IO_URING

#include <linux/io_uring.h>
#include <pthread.h>
#include <poll.h>
#include <sys/socket.h>
#include <sys/mman.h>
#include <sys/syscall.h>
#include <string.h>

#define URING_ENTRIES 1024
#define URING_RBUF_SIZE 16384
#define URING_POOL_BUFS 1024 /* power of 2 */
#define URING_WBUF_MAX (1024 * 1024)
#define URING_WBUF_KEEP 65536 /* free a bigger buffer when idle */

#define U_IDLE 0
#define U_INFLIGHT 1
#define U_READY 2

typedef struct rktio_uring_fd_t {
  intptr_t fd;
  int closed;      /* rktio_fd_t is gone; free when no ops in flight */
  int owns_fd;     /* closed while a send was in flight: close fd when done */
  /* ltps signals found while reaping, deferred to a context that
     holds the sleep-relevant lock (see uring_flush_signals_locked) */
  int sig_r, sig_w, on_sig;
  struct rktio_uring_fd_t *sig_next;
  int shut_wr;     /* shutdown(SHUT_WR) requested while a send was in flight */
  int in_flight;   /* number of SQEs queued or submitted */
  rktio_ltps_t *lt;
  /* reading: */
  char *rbuf;       /* from the ring's buffer pool, if it has one */
  int r_bid;        /* pool buffer id, or -1 */
  int r_state;
  intptr_t r_res, r_pos;
  /* writing: */
  char *wbuf;
  intptr_t wbuf_size;
  int w_state;
  intptr_t w_off, w_len;
  int w_err;
} rktio_uring_fd_t;

/* A multishot poll watching a listening socket, so that checking
   whether a connection is waiting needs no system call until the
   kernel reports activity */
typedef struct rktio_uring_watch_t {
  intptr_t fd;
  int armed, maybe_ready, closed;
} rktio_uring_watch_t;
#define TAG_WATCH_BIT 2

typedef struct rktio_uring_t {
  int fd;
  unsigned *sq_tail, *sq_mask, *sq_array, *sq_head;
  unsigned *cq_head, *cq_tail, *cq_mask, *sq_flags;
  int coop;            /* IORING_SETUP_COOP_TASKRUN | TASKRUN_FLAG */
  int n_ops;           /* socket recvs and sends in flight */
  /* Reaping can happen in any rktio context, including a Racket
     parallel thread holding only the rktio lock, while the long-term
     poll set (ltps) may be used only with the sleep-relevant lock;
     so reaping records signals here, and they are delivered by
     contexts with that lock (ltps poll, ltps add, sleep) */
  struct rktio_uring_fd_t *sig_list;
  int deferred_hits;   /* listener activity not yet reported */
  int sleeping;        /* a sleep is waiting on the ring */
  rktio_t *rktio;
  int spin_credit, spin_skips; /* see rktio_uring_spin */
  /* Receive buffer pool shared by all sockets (a "provided buffer
     ring"): the kernel picks a buffer when data arrives, so a
     connection that is waiting for data holds no buffer */
  struct io_uring_buf_ring *br;
  char *pool;
  unsigned br_tail;
  struct io_uring_sqe *sqes;
  struct io_uring_cqe *cqes;
  unsigned sq_entries;
  unsigned local_tail; /* SQEs filled but not yet published+submitted */
  int pending;
  pthread_mutex_t lock;
  struct rktio_uring_t *next_global;
  /* one-shot poll on the long-term poll set's epoll fd, so that
     checking the long-term poll set can skip epoll_wait() */
  int lt_fd, lt_armed, lt_fired;
} rktio_uring_t;

#define TAG_LT_POLL 2

struct rktio_uring_fd_t;
#define UFD(rfd) ((rktio_uring_fd_t *)(rfd)->u)

static int uring_mode = -1;
static rktio_uring_t *all_rings = NULL;
static pthread_mutex_t all_rings_lock = PTHREAD_MUTEX_INITIALIZER;

static void uring_flush_at_exit(void);

/* lock held, or ring not yet shared */
static void uring_pool_return(rktio_uring_t *u, int bid)
{
  struct io_uring_buf *b = &u->br->bufs[u->br_tail & (URING_POOL_BUFS - 1)];
  b->addr = (uintptr_t)(u->pool + (size_t)bid * URING_RBUF_SIZE);
  b->len = URING_RBUF_SIZE;
  b->bid = bid;
  u->br_tail++;
  __atomic_store_n(&u->br->tail, (unsigned short)u->br_tail, __ATOMIC_RELEASE);
}

/* lock held */
static void uring_release_rbuf(rktio_uring_t *u, rktio_uring_fd_t *ufd)
{
  if (ufd->r_bid >= 0) {
    uring_pool_return(u, ufd->r_bid);
    ufd->r_bid = -1;
    ufd->rbuf = NULL;
  }
}

static rktio_uring_t *get_uring(rktio_t *rktio)
{
  rktio_uring_t *u;
  struct io_uring_params p;
  void *sq;
  size_t sz, cq_sz;
  int fd;

  if (rktio->uring)
    return (rktio->uring->fd >= 0) ? rktio->uring : NULL;

  u = calloc(1, sizeof(rktio_uring_t));
  u->fd = -1;
  rktio->uring = u;

  memset(&p, 0, sizeof(p));
  {
    /* Cooperative task running avoids interrupting the thread when
       an operation completes asynchronously; the kernel instead
       sets IORING_SQ_TASKRUN, and we enter the kernel to finish
       completions when we look for them */
    const char *s = getenv("PLT_RKTIO_URING_COOP");
    if (!s || strcmp(s, "0"))
      p.flags = IORING_SETUP_COOP_TASKRUN | IORING_SETUP_TASKRUN_FLAG;
  }
  fd = syscall(__NR_io_uring_setup, URING_ENTRIES, &p);
  if ((fd < 0) && p.flags) {
    memset(&p, 0, sizeof(p));
    fd = syscall(__NR_io_uring_setup, URING_ENTRIES, &p);
  }
  if (fd < 0) return NULL;
  u->coop = (p.flags & IORING_SETUP_TASKRUN_FLAG) ? 1 : 0;
  if (!(p.features & IORING_FEAT_SINGLE_MMAP)) { close(fd); return NULL; }

  sz = p.sq_off.array + p.sq_entries * sizeof(unsigned);
  cq_sz = p.cq_off.cqes + p.cq_entries * sizeof(struct io_uring_cqe);
  if (cq_sz > sz) sz = cq_sz;
  sq = mmap(NULL, sz, PROT_READ | PROT_WRITE, MAP_SHARED | MAP_POPULATE, fd, IORING_OFF_SQ_RING);
  if (sq == MAP_FAILED) { close(fd); return NULL; }
  u->sqes = mmap(NULL, p.sq_entries * sizeof(struct io_uring_sqe), PROT_READ | PROT_WRITE,
                 MAP_SHARED | MAP_POPULATE, fd, IORING_OFF_SQES);
  if (u->sqes == MAP_FAILED) { close(fd); return NULL; }

  u->sq_head = (unsigned *)((char *)sq + p.sq_off.head);
  u->sq_tail = (unsigned *)((char *)sq + p.sq_off.tail);
  u->sq_mask = (unsigned *)((char *)sq + p.sq_off.ring_mask);
  u->sq_array = (unsigned *)((char *)sq + p.sq_off.array);
  u->sq_flags = (unsigned *)((char *)sq + p.sq_off.flags);
  u->cq_head = (unsigned *)((char *)sq + p.cq_off.head);
  u->cq_tail = (unsigned *)((char *)sq + p.cq_off.tail);
  u->cq_mask = (unsigned *)((char *)sq + p.cq_off.ring_mask);
  u->cqes = (struct io_uring_cqe *)((char *)sq + p.cq_off.cqes);
  u->sq_entries = p.sq_entries;
  u->local_tail = *u->sq_tail;
  u->lt_fd = -1;
  pthread_mutex_init(&u->lock, NULL);
  rktio_fd_cloexec(fd);
  u->fd = fd;
  u->rktio = rktio;

  {
    /* register the receive buffer pool; kernels before 5.19 don't
       support it, and then each socket gets its own buffer */
    struct io_uring_buf_reg reg;
    size_t br_sz = URING_POOL_BUFS * sizeof(struct io_uring_buf);
    void *br = mmap(NULL, br_sz, PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
    if (br != MAP_FAILED) {
      memset(&reg, 0, sizeof(reg));
      reg.ring_addr = (uintptr_t)br;
      reg.ring_entries = URING_POOL_BUFS;
      reg.bgid = 0;
      if (!syscall(__NR_io_uring_register, fd, IORING_REGISTER_PBUF_RING, &reg, 1)) {
        int i;
        u->br = br;
        u->pool = malloc((size_t)URING_POOL_BUFS * URING_RBUF_SIZE);
        for (i = 0; i < URING_POOL_BUFS; i++)
          uring_pool_return(u, i);
      } else
        munmap(br, br_sz);
    }
  }

  pthread_mutex_lock(&all_rings_lock);
  if (!all_rings)
    atexit(uring_flush_at_exit);
  u->next_global = all_rings;
  all_rings = u;
  pthread_mutex_unlock(&all_rings_lock);

  return u;
}

/* The kernel ties each io_uring request to the OS thread that
   submitted it, and it cancels a thread's pending requests when that
   thread exits; a Racket parallel thread pool's worker threads can
   exit at any time, so only the thread that owns the rktio_t (a
   place's main thread, which runs the scheduler's sleep and polls)
   submits. Other threads leave SQEs queued and wake the owner if
   it's sleeping. Returns 1 if the current thread may submit. */
static int uring_may_submit(rktio_uring_t *u)
{
  if (pthread_equal(pthread_self(), u->rktio->uring_owner))
    return 1;
  if (u->sleeping) {
    u->sleeping = 0;
    rktio_signal_received(u->rktio);
  }
  return 0;
}

/* lock held */
static int uring_submit_locked(rktio_uring_t *u)
{
  int r;
  if (!u->pending)
    return 0;
  if (!uring_may_submit(u)
      && ((u->local_tail - __atomic_load_n(u->sq_head, __ATOMIC_ACQUIRE)) < u->sq_entries))
    return 0; /* (if the SQ is full, submit anyway) */
  __atomic_store_n(u->sq_tail, u->local_tail, __ATOMIC_RELEASE);
  do {
    r = syscall(__NR_io_uring_enter, u->fd, u->pending, 0, 0, NULL, 0);
  } while ((r < 0) && (errno == EINTR));
  if (r > 0)
    u->pending -= r;
  return r;
}

/* lock held; returns an SQE to fill */
static struct io_uring_sqe *uring_get_sqe(rktio_uring_t *u)
{
  struct io_uring_sqe *sqe;
  unsigned idx;

  if ((u->local_tail - __atomic_load_n(u->sq_head, __ATOMIC_ACQUIRE)) >= u->sq_entries)
    uring_submit_locked(u);

  idx = u->local_tail & *u->sq_mask;
  sqe = &u->sqes[idx];
  memset(sqe, 0, sizeof(*sqe));
  u->sq_array[idx] = idx;
  u->local_tail++;
  u->pending++;
  /* a non-owner thread can't submit, so make sure the owner wakes
     up to submit this */
  if (u->sleeping && !pthread_equal(pthread_self(), u->rktio->uring_owner)) {
    u->sleeping = 0;
    rktio_signal_received(u->rktio);
  }
  return sqe;
}

static void uring_queue_recv(rktio_uring_t *u, rktio_uring_fd_t *ufd)
{
  struct io_uring_sqe *sqe = uring_get_sqe(u);
  sqe->opcode = IORING_OP_RECV;
  sqe->fd = ufd->fd;
  if (u->br) {
    sqe->flags = IOSQE_BUFFER_SELECT;
    sqe->buf_group = 0;
  } else {
    if (!ufd->rbuf) ufd->rbuf = malloc(URING_RBUF_SIZE);
    sqe->addr = (uintptr_t)ufd->rbuf;
  }
  sqe->len = URING_RBUF_SIZE;
  sqe->user_data = (uintptr_t)ufd;
  ufd->r_state = U_INFLIGHT;
  ufd->in_flight++;
  u->n_ops++;
}

static void uring_queue_send(rktio_uring_t *u, rktio_uring_fd_t *ufd)
{
  struct io_uring_sqe *sqe = uring_get_sqe(u);
  sqe->opcode = IORING_OP_SEND;
  sqe->fd = ufd->fd;
  sqe->addr = (uintptr_t)(ufd->wbuf + ufd->w_off);
  sqe->len = ufd->w_len;
  sqe->msg_flags = MSG_NOSIGNAL;
  sqe->user_data = (uintptr_t)ufd | 1;
  ufd->w_state = U_INFLIGHT;
  ufd->in_flight++;
  u->n_ops++;
}

static void uring_queue_cancel(rktio_uring_t *u, uintptr_t target)
{
  struct io_uring_sqe *sqe = uring_get_sqe(u);
  sqe->opcode = IORING_OP_ASYNC_CANCEL;
  sqe->fd = -1;
  sqe->addr = target;
  sqe->user_data = 0; /* ignored */
}

/* lock held; returns number of ltps handles signaled */
static int uring_reap_locked(rktio_uring_t *u)
{
  unsigned head, tail;
  int hits = 0;

  if (u->coop && (__atomic_load_n(u->sq_flags, __ATOMIC_ACQUIRE) & IORING_SQ_TASKRUN)) {
    /* run deferred completion work so that its CQEs are posted */
    syscall(__NR_io_uring_enter, u->fd, 0, 0, IORING_ENTER_GETEVENTS, NULL, 0);
  }

  head = *u->cq_head;

  tail = __atomic_load_n(u->cq_tail, __ATOMIC_ACQUIRE);
  while (head != tail) {
    struct io_uring_cqe *cqe = &u->cqes[head & *u->cq_mask];
    uintptr_t ud = cqe->user_data;
    int res = cqe->res;
    head++;
    if (ud == TAG_LT_POLL) {
      u->lt_armed = 0;
      u->lt_fired = 1;
    } else if (ud & TAG_WATCH_BIT) {
      rktio_uring_watch_t *w = (rktio_uring_watch_t *)(ud & ~(uintptr_t)15);
      if (!(cqe->flags & IORING_CQE_F_MORE)) {
        w->armed = 0;
        if (w->closed) {
          free(w);
          continue;
        }
      }
      w->maybe_ready = 1;
      /* report as a hit, so that a sleep that reaps this doesn't
         start waiting with the listener's activity unreported */
      u->deferred_hits++;
    } else if (ud) {
      rktio_uring_fd_t *ufd = (rktio_uring_fd_t *)(ud & ~(uintptr_t)15);
      int is_write = (int)(ud & 1);
      ufd->in_flight--;
      u->n_ops--;
      if (!is_write && (cqe->flags & IORING_CQE_F_BUFFER)) {
        ufd->r_bid = cqe->flags >> IORING_CQE_BUFFER_SHIFT;
        ufd->rbuf = u->pool + (size_t)ufd->r_bid * URING_RBUF_SIZE;
      }
      if (ufd->closed) {
        if (!is_write) uring_release_rbuf(u, ufd);
        if (is_write && (res > 0) && (res < ufd->w_len)) {
          /* finish sending data written before the port was closed */
          ufd->w_off += res;
          ufd->w_len -= res;
          uring_queue_send(u, ufd);
          continue;
        }
        if (is_write && ufd->shut_wr) {
          ufd->shut_wr = 0;
          shutdown(ufd->fd, SHUT_WR);
        }
        if (!ufd->in_flight) {
          if (ufd->owns_fd) rktio_reliably_close(ufd->fd);
          if (ufd->rbuf && !u->br) free(ufd->rbuf);
          if (ufd->wbuf) free(ufd->wbuf);
          free(ufd);
        }
        continue;
      }
      if ((res == -ECANCELED) && is_write) {
        /* The kernel cancels a thread's pending operations when the
           OS thread that submitted them exits, as a Racket parallel
           thread pool's worker can; the socket is still open, so try
           again (the data is still in the buffer) */
        uring_queue_send(u, ufd);
        continue;
      }
      if (!is_write) {
        if ((res == -ENOBUFS) || (res == -ECANCELED)) {
          /* pool exhausted, or canceled by a submitting thread's exit
             (see above); the reader will try again */
          ufd->r_state = U_IDLE;
        } else {
          ufd->r_state = U_READY;
          ufd->r_res = res;
          ufd->r_pos = 0;
        }
      } else {
        if (res < 0) {
          ufd->w_err = -res;
          ufd->w_state = U_IDLE;
        } else if (res < ufd->w_len) {
          ufd->w_off += res;
          ufd->w_len -= res;
          uring_queue_send(u, ufd);
          continue; /* still in flight */
        } else {
          ufd->w_state = U_IDLE;
          if (ufd->wbuf_size > URING_WBUF_KEEP) {
            free(ufd->wbuf);
            ufd->wbuf = NULL;
            ufd->wbuf_size = 0;
          }
        }
        if (ufd->shut_wr) {
          ufd->shut_wr = 0;
          shutdown(ufd->fd, SHUT_WR);
        }
      }
      if (ufd->lt) {
        if (is_write) ufd->sig_w = 1; else ufd->sig_r = 1;
        if (!ufd->on_sig) {
          ufd->on_sig = 1;
          ufd->sig_next = u->sig_list;
          u->sig_list = ufd;
        }
      }
    }
    if (head == tail)
      tail = __atomic_load_n(u->cq_tail, __ATOMIC_ACQUIRE);
  }
  __atomic_store_n(u->cq_head, head, __ATOMIC_RELEASE);

  if (u->sleeping && (u->sig_list || u->deferred_hits)) {
    /* we consumed completions that a sleeping scheduler (in another
       OS thread) was waiting for, so wake it up */
    u->sleeping = 0;
    rktio_signal_received(u->rktio);
  }

  return hits;
}

/* lock held, and caller has the sleep-relevant lock; returns the
   number of ltps handles signaled plus unreported listener activity */
static int uring_flush_signals_locked(rktio_uring_t *u)
{
  int hits = u->deferred_hits;
  rktio_uring_fd_t *ufd;
  u->deferred_hits = 0;
  while ((ufd = u->sig_list)) {
    u->sig_list = ufd->sig_next;
    ufd->on_sig = 0;
    if (ufd->lt) {
      if (ufd->sig_r) hits += rktio_ltps_signal_fd(ufd->lt, ufd->fd, 0);
      if (ufd->sig_w) hits += rktio_ltps_signal_fd(ufd->lt, ufd->fd, 1);
    }
    ufd->sig_r = ufd->sig_w = 0;
  }
  return hits;
}

/* lock held */
static void uring_unlist_signals(rktio_uring_t *u, rktio_uring_fd_t *ufd)
{
  if (ufd->on_sig) {
    rktio_uring_fd_t **p = &u->sig_list;
    while (*p != ufd) p = &(*p)->sig_next;
    *p = ufd->sig_next;
    ufd->on_sig = 0;
  }
}

/* For rktio_sleep, with the sleep-relevant lock: returns nonzero if
   the sleep should be skipped; otherwise, the sleep must be followed
   by rktio_uring_end_sleep */
int rktio_uring_prepare_sleep(rktio_t *rktio)
{
  rktio_uring_t *u = rktio->uring;
  int hits;
  pthread_mutex_lock(&u->lock);
  uring_submit_locked(u);
  hits = uring_reap_locked(u);
  hits += uring_flush_signals_locked(u);
  if (u->pending)
    uring_submit_locked(u);
  if (!hits)
    u->sleeping = 1;
  pthread_mutex_unlock(&u->lock);
  return hits;
}

void rktio_uring_end_sleep(rktio_t *rktio)
{
  rktio_uring_t *u = rktio->uring;
  pthread_mutex_lock(&u->lock);
  u->sleeping = 0;
  pthread_mutex_unlock(&u->lock);
}

int rktio_uring_submit_and_reap(rktio_t *rktio)
{
  rktio_uring_t *u = rktio->uring;
  int hits;
  if (!u || (u->fd < 0)) return 0;
  pthread_mutex_lock(&u->lock);
  uring_submit_locked(u);
  hits = uring_reap_locked(u);
  hits += uring_flush_signals_locked(u);
  if (u->pending) /* re-sends queued by reaping */
    uring_submit_locked(u);
  pthread_mutex_unlock(&u->lock);
  return hits;
}

/* Before sleeping, briefly watch the completion queue (shared
   memory, so no system calls) for operations that finish soon, such
   as a request--response exchange on a single connection; that avoids
   a sleep and wakeup in the kernel. Returns nonzero if it found
   anything. */
int rktio_uring_spin(rktio_t *rktio)
{
  static int spin_usecs = -1;
  rktio_uring_t *u = rktio->uring;
  struct timespec start, now;
  long elapsed;
  int i;

  if (spin_usecs < 0) {
    const char *s = getenv("PLT_RKTIO_URING_SPIN");
    spin_usecs = (s && *s) ? atoi(s) : 50;
  }
  if (!spin_usecs || !u || (u->fd < 0) || !u->n_ops)
    return 0;

  /* Back off when spinning doesn't pay: spin while it recently found
     completions, and otherwise only on every 32nd sleep, so that a
     mostly idle process with open connections doesn't burn CPU */
  if (u->spin_credit <= 0) {
    if ((++u->spin_skips & 31) != 0)
      return 0;
  }

  clock_gettime(CLOCK_MONOTONIC, &start);
  while (1) {
    for (i = 0; i < 64; i++) {
      if ((*u->cq_head != __atomic_load_n(u->cq_tail, __ATOMIC_ACQUIRE))
          || (u->coop && (__atomic_load_n(u->sq_flags, __ATOMIC_ACQUIRE) & IORING_SQ_TASKRUN)))
      {
        u->spin_credit = 16;
        return rktio_uring_submit_and_reap(rktio) + 1;
      }
#if defined(__x86_64__) || defined(__i386__)
      __builtin_ia32_pause();
#elif defined(__aarch64__)
      __asm__ __volatile__("yield");
#endif
    }
    clock_gettime(CLOCK_MONOTONIC, &now);
    elapsed = (now.tv_sec - start.tv_sec) * 1000000 + (now.tv_nsec - start.tv_nsec) / 1000;
    if (elapsed >= spin_usecs) {
      if (u->spin_credit > 0) u->spin_credit--;
      return 0;
    }
  }
}

/* lock held */
static void uring_queue_poll(rktio_uring_t *u, int fd, uintptr_t tag)
{
  struct io_uring_sqe *sqe = uring_get_sqe(u);
  sqe->opcode = IORING_OP_POLL_ADD;
  sqe->fd = fd;
  sqe->poll32_events = POLLIN;
  sqe->user_data = tag;
}

rktio_uring_watch_t *rktio_uring_watch_new(rktio_t *rktio, intptr_t fd)
{
  rktio_uring_watch_t *w;
  if (uring_mode < 0) {
    const char *s = getenv("PLT_RKTIO_URING");
    uring_mode = (s && *s && strcmp(s, "0")) ? 1 : 0;
  }
  if (!uring_mode || !get_uring(rktio))
    return NULL;
  w = calloc(1, sizeof(rktio_uring_watch_t));
  w->fd = fd;
  w->maybe_ready = 1;
  return w;
}

/* lock held */
static void uring_watch_arm(rktio_uring_t *u, rktio_uring_watch_t *w)
{
  if (!w->armed) {
    struct io_uring_sqe *sqe = uring_get_sqe(u);
    sqe->opcode = IORING_OP_POLL_ADD;
    sqe->fd = w->fd;
    sqe->poll32_events = POLLIN;
    sqe->len = IORING_POLL_ADD_MULTI;
    sqe->user_data = (uintptr_t)w | TAG_WATCH_BIT;
    w->armed = 1;
    /* we can't know the state between now and the poll's start */
    w->maybe_ready = 1;
  }
}

/* Returns 0 if the watched fd is known not to be ready; 1 means that
   the caller must check (and report back via rktio_uring_watch_ready) */
int rktio_uring_watch_check(rktio_t *rktio, rktio_uring_watch_t *w)
{
  rktio_uring_t *u = rktio->uring;
  int maybe;
  pthread_mutex_lock(&u->lock);
  uring_reap_locked(u);
  uring_watch_arm(u, w);
  maybe = w->maybe_ready;
  w->maybe_ready = 0;
  pthread_mutex_unlock(&u->lock);
  return maybe;
}

void rktio_uring_watch_ready(rktio_t *rktio, rktio_uring_watch_t *w)
{
  /* stays ready (e.g., more connections may be waiting) until a check
     finds otherwise */
  w->maybe_ready = 1;
}

/* Adds the ring's fd to a poll set in place of the watched fd */
void rktio_uring_watch_poll_add(rktio_t *rktio, rktio_uring_watch_t *w, rktio_poll_set_t *fds)
{
  rktio_uring_t *u = rktio->uring;
  pthread_mutex_lock(&u->lock);
  uring_watch_arm(u, w);
  pthread_mutex_unlock(&u->lock);
  RKTIO_FD_SET(u->fd, fds);
}

/* The watched fd is about to be closed */
void rktio_uring_watch_free(rktio_t *rktio, rktio_uring_watch_t *w)
{
  rktio_uring_t *u = rktio->uring;
  pthread_mutex_lock(&u->lock);
  uring_reap_locked(u);
  if (w->armed) {
    struct io_uring_sqe *sqe = uring_get_sqe(u);
    sqe->opcode = IORING_OP_POLL_REMOVE;
    sqe->fd = -1;
    sqe->addr = (uintptr_t)w | TAG_WATCH_BIT;
    sqe->user_data = 0;
    w->closed = 1; /* freed when the poll's final CQE arrives */
    uring_submit_locked(u);
  } else
    free(w);
  pthread_mutex_unlock(&u->lock);
}

/* Called by the long-term poll set before epoll_wait(); returns 1 if
   the epoll fd is known to have no events, so epoll_wait() can be skipped */
int rktio_uring_ltps_skip(rktio_t *rktio, int lt_fd)
{
  rktio_uring_t *u = rktio->uring;
  int skip;
  if (!u || (u->fd < 0)) return 0;
  pthread_mutex_lock(&u->lock);
  skip = ((u->lt_fd == lt_fd) && u->lt_armed && !u->lt_fired);
  pthread_mutex_unlock(&u->lock);
  return skip;
}

/* Called by the long-term poll set after draining epoll */
void rktio_uring_ltps_rearm(rktio_t *rktio, int lt_fd)
{
  rktio_uring_t *u = rktio->uring;
  if (!u || (u->fd < 0)) return;
  pthread_mutex_lock(&u->lock);
  if ((u->lt_fd != lt_fd) || !u->lt_armed) {
    u->lt_fd = lt_fd;
    u->lt_armed = 1;
    uring_queue_poll(u, lt_fd, TAG_LT_POLL);
  }
  u->lt_fired = 0;
  pthread_mutex_unlock(&u->lock);
}

int rktio_uring_get_fd(rktio_t *rktio)
{
  return (rktio->uring ? rktio->uring->fd : -1);
}

static void uring_flush_at_exit(void)
{
  /* Make sure that queued sends reach the kernel and finish */
  rktio_uring_t *u;
  for (u = all_rings; u; u = u->next_global) {
    int tries;
    if (u->fd < 0) continue;
    pthread_mutex_lock(&u->lock);
    uring_submit_locked(u);
    for (tries = 0; tries < 100; tries++) {
      struct pollfd pfd;
      uring_reap_locked(u);
      if (u->pending) uring_submit_locked(u);
      if (*u->cq_head == __atomic_load_n(u->cq_tail, __ATOMIC_ACQUIRE)) {
        /* wait a little for outstanding completions (e.g., sends) */
        pfd.fd = u->fd;
        pfd.events = POLLIN;
        if (poll(&pfd, 1, 20) <= 0)
          break;
      }
    }
    pthread_mutex_unlock(&u->lock);
  }
}

void rktio_uring_destroy(rktio_t *rktio)
{
  /* Prototype: ring stays allocated (and listed for exit flushing) */
}

void rktio_fd_enable_uring(rktio_t *rktio, rktio_fd_t *rfd)
{
  rktio_uring_fd_t *ufd;

  if (uring_mode < 0) {
    const char *s = getenv("PLT_RKTIO_URING");
    uring_mode = (s && *s && strcmp(s, "0")) ? 1 : 0;
  }
  if (!uring_mode)
    return;
  if (!get_uring(rktio))
    return;

  ufd = calloc(1, sizeof(rktio_uring_fd_t));
  ufd->fd = rfd->fd;
  ufd->r_bid = -1;
  rfd->u = ufd;
}

int rktio_fd_uses_uring(rktio_fd_t *rfd)
{
  return rfd->u != NULL;
}

static int uring_direct_recv = -1;

static intptr_t uring_read(rktio_t *rktio, rktio_fd_t *rfd, char *buffer, intptr_t len, rktio_err_t *err)
{
  rktio_uring_t *u = rktio->uring;
  rktio_uring_fd_t *ufd = UFD(rfd);
  intptr_t n;
  int tried = 0, mode = 0;

  if (uring_direct_recv < 0) {
    const char *s = getenv("PLT_RKTIO_URING_DIRECT");
    uring_direct_recv = (s && *s) ? atoi(s) : 3;
  }

  pthread_mutex_lock(&u->lock);
  uring_reap_locked(u);
 again:
  switch (ufd->r_state) {
  case U_READY:
    if (ufd->r_res > 0) {
      n = ufd->r_res - ufd->r_pos;
      if (n > len) n = len;
      memcpy(buffer, ufd->rbuf + ufd->r_pos, n);
      ufd->r_pos += n;
      if (ufd->r_pos == ufd->r_res) {
        ufd->r_state = U_IDLE;
        uring_release_rbuf(u, ufd);
      }
    } else if (ufd->r_res == 0)
      n = RKTIO_READ_EOF; /* stays at EOF */
    else {
      errno = -ufd->r_res;
      rktio_get_posix_error(err);
      ufd->r_state = U_IDLE;
      n = RKTIO_READ_ERROR;
    }
    break;
  case U_IDLE:
    if (!tried) {
      /* Choose how to start the read. Mode 3 (the default) picks:
         when this socket's own send is the only other operation in
         flight, as for a single busy connection, submit the recv
         together with that send (mode 1); otherwise, keep batching,
         but try a direct recv when little is queued (mode 2) */
      if (uring_direct_recv == 3)
        mode = (u->n_ops <= ufd->in_flight + 1) ? 1 : 2;
      else
        mode = uring_direct_recv;
    }
    if ((mode == 2) && (u->pending <= 1)) {
      /* Little to batch with: submit now (including this socket's
         queued send, if any) and try a direct recv */
      uring_submit_locked(u);
      do {
        n = recv(ufd->fd, buffer, len, MSG_DONTWAIT);
      } while ((n == -1) && (errno == EINTR));
      if (n > 0)
        break;
      else if (n == 0) {
        ufd->r_state = U_READY; /* sticky EOF */
        ufd->r_res = 0;
        n = RKTIO_READ_EOF;
        break;
      } else if ((errno != EAGAIN) && (errno != EWOULDBLOCK)) {
        rktio_get_posix_error(err);
        n = RKTIO_READ_ERROR;
        break;
      }
    }
    uring_queue_recv(u, ufd);
    if ((mode == 1) && (u->pending <= 2) && !tried) {
      /* Little to batch with: submit the recv now, together with
         this socket's queued send (if any). If data is already
         there, the kernel completes the recv during submission, so
         the read finishes without blocking the Racket thread, and
         a request--response exchange costs one system call */
      tried = 1;
      uring_submit_locked(u);
      uring_reap_locked(u);
      if (ufd->r_state == U_READY)
        goto again;
    }
    n = 0;
    break;
  default:
    n = 0;
    break;
  }
  pthread_mutex_unlock(&u->lock);
  return n;
}

static intptr_t uring_write(rktio_t *rktio, rktio_fd_t *rfd, const char *buffer, intptr_t len, rktio_err_t *err)
{
  rktio_uring_t *u = rktio->uring;
  rktio_uring_fd_t *ufd = UFD(rfd);
  intptr_t n;

  pthread_mutex_lock(&u->lock);
  uring_reap_locked(u);
  if (ufd->w_err) {
    errno = ufd->w_err;
    ufd->w_err = 0;
    rktio_get_posix_error(err);
    n = RKTIO_WRITE_ERROR;
  } else if (ufd->w_state == U_INFLIGHT)
    n = 0;
  else {
    /* size the buffer to the write, so that idle connections and
       small writes stay small, but a large write is one send */
    n = (len > URING_WBUF_MAX) ? URING_WBUF_MAX : len;
    if (ufd->wbuf_size < n) {
      intptr_t sz = (n + 4095) & ~(intptr_t)4095;
      if (ufd->wbuf) free(ufd->wbuf);
      ufd->wbuf = malloc(sz);
      ufd->wbuf_size = sz;
    }
    memcpy(ufd->wbuf, buffer, n);
    ufd->w_off = 0;
    ufd->w_len = n;
    if (n > 0)
      uring_queue_send(u, ufd);
  }
  pthread_mutex_unlock(&u->lock);
  return n;
}

static int uring_poll_read_ready(rktio_t *rktio, rktio_fd_t *rfd)
{
  rktio_uring_t *u = rktio->uring;
  rktio_uring_fd_t *ufd = UFD(rfd);
  int r;
  pthread_mutex_lock(&u->lock);
  uring_reap_locked(u);
  if (ufd->r_state == U_IDLE)
    uring_queue_recv(u, ufd);
  r = (ufd->r_state == U_READY) ? RKTIO_POLL_READY : RKTIO_POLL_NOT_READY;
  pthread_mutex_unlock(&u->lock);
  return r;
}

static int uring_poll_write_ready(rktio_t *rktio, rktio_fd_t *rfd)
{
  rktio_uring_t *u = rktio->uring;
  rktio_uring_fd_t *ufd = UFD(rfd);
  int r;
  pthread_mutex_lock(&u->lock);
  uring_reap_locked(u);
  r = ((ufd->w_state == U_IDLE) ? RKTIO_POLL_READY : RKTIO_POLL_NOT_READY);
  pthread_mutex_unlock(&u->lock);
  return r;
}

void rktio_uring_ltps_watch(rktio_t *rktio, rktio_ltps_t *lt, rktio_fd_t *rfd, int is_write)
{
  rktio_uring_t *u = rktio->uring;
  rktio_uring_fd_t *ufd = UFD(rfd);
  int ready;
  pthread_mutex_lock(&u->lock);
  ufd->lt = lt;
  uring_reap_locked(u);
  (void)uring_flush_signals_locked(u);
  if (!is_write) {
    if (ufd->r_state == U_IDLE)
      uring_queue_recv(u, ufd);
    ready = (ufd->r_state == U_READY);
  } else
    ready = (ufd->w_state == U_IDLE);
  if (ready)
    rktio_ltps_signal_fd(lt, ufd->fd, is_write);
  pthread_mutex_unlock(&u->lock);
}

/* Returns 1 if a shutdown for writing is deferred until the
   in-flight send completes */
int rktio_uring_shutdown_wr(rktio_t *rktio, rktio_fd_t *rfd)
{
  rktio_uring_t *u = rktio->uring;
  rktio_uring_fd_t *ufd = UFD(rfd);
  int deferred = 0;
  if (!ufd || !u) return 0;
  pthread_mutex_lock(&u->lock);
  uring_reap_locked(u);
  if (ufd->w_state == U_INFLIGHT) {
    ufd->shut_wr = 1;
    deferred = 1;
  }
  pthread_mutex_unlock(&u->lock);
  return deferred;
}

/* rfd is about to be closed (`closing`) or forgotten; returns 1 if
   the ring takes over closing the fd, because a send is still in
   flight and the data written to the port must still be delivered */
static int uring_release(rktio_t *rktio, rktio_fd_t *rfd, int closing)
{
  rktio_uring_t *u = rktio ? rktio->uring : NULL;
  rktio_uring_fd_t *ufd = UFD(rfd);
  int owns = 0;

  rfd->u = NULL;
  if (!u) return 0; /* shouldn't happen */

  pthread_mutex_lock(&u->lock);
  uring_reap_locked(u);
  ufd->closed = 1;
  uring_unlist_signals(u, ufd);
  uring_release_rbuf(u, ufd);
  ufd->lt = NULL;
  if (ufd->r_state == U_INFLIGHT)
    uring_queue_cancel(u, (uintptr_t)ufd);
  if (closing
      && ((ufd->w_state == U_INFLIGHT)
          /* a non-owner thread can't submit (see uring_may_submit),
             so queued SQEs may still refer to the fd: */
          || (ufd->in_flight && !pthread_equal(pthread_self(), rktio->uring_owner)))) {
    ufd->owns_fd = 1;
    owns = 1;
  }
  /* SQEs must be submitted before the fd is closed, since the
     kernel resolves the fd at submission */
  uring_submit_locked(u);
  if (!ufd->in_flight) {
    owns = 0; /* nothing in flight, so the caller closes */
    if (ufd->rbuf && !u->br) free(ufd->rbuf);
    if (ufd->wbuf) free(ufd->wbuf);
    free(ufd);
  }
  pthread_mutex_unlock(&u->lock);
  return owns;
}

#endif

/*========================================================================*/
/* Windows I/O helper structs                                             */
/*========================================================================*/

#ifdef RKTIO_SYSTEM_WINDOWS

/* There doesn't appear to be a good answer for fd-style waiting on
   input with a Windows console handle.

   When USE_BACKGROUND_THREAD_FOR_CONSOLE_INPUT is 0, the
   implementation here treats the input as ready as soon as there are
   events in the console, but those might be key events that haven't
   been submitted by hitting Return. So, a `sync` could return too
   early and reading can block when it shouldn't.

   When USE_BACKGROUND_THREAD_FOR_CONSOLE_INPUT is 1, a background
   thread avoids that problem with blocking, but can consume
   characters while waiting, which means the console handle can't be
   shared (with expeditor, for example). */
#define USE_BACKGROUND_THREAD_FOR_CONSOLE_INPUT 0

typedef struct Win_FD_Input_Thread {
  /* This is malloced for use in a Win32 thread */
  HANDLE fd;
  int avail, offset, err, checking, you_clean_up;
  int *refcount;
  HANDLE eof;
  unsigned char *buffer;
  HANDLE lock_sema, checking_sema, ready_sema;
  HANDLE thread;
} Win_FD_Input_Thread;

typedef struct Win_FD_Output_Thread {
  /* This is malloced for use in a Win32 thread */
  HANDLE fd;
  int nonblocking;  /* non-zero => an NT pipe where non-blocking WriteFile
		       works. We still use a thread to detect when the
		       write has ben flushed, which in turn is needed to
		       know whether future writes will immediately succeed. */
  int flushed, needflush; /* Used for non-blocking, only. The flushed
			     flag communicates from the flush-testing thread
			     to the main thread. For efficiency, we request
			     flush checking only when needed (instead of
			     after every write); needflush indicates that
			     a flush check is currently needed, but hasn't
			     been started. */
  int done, err_no, you_clean_up;
  unsigned int buflen; /* used for blocking, only */
  unsigned char *buffer; /* used for blocking, only */
  int *refcount;
  HANDLE lock_sema, work_sema, ready_sema;
  HANDLE thread;
} Win_FD_Output_Thread;

# define RKTIO_FD_BUFFSIZE 4096

static void init_read_fd(rktio_t *rktio, rktio_fd_t *rfd);
static void deinit_read_fd(rktio_fd_t *rfd, int full_close);
static void deinit_write_fd(rktio_fd_t *rfd, int full_close);

static void deinit_fd(rktio_fd_t *rfd, int full_close)
{
  deinit_read_fd(rfd, full_close);
  deinit_write_fd(rfd, full_close);
}

#define DELAYED_CONSOLE_HANDLE ((HANDLE)-2)
static void force_console(rktio_fd_t *rfd);
static HANDLE get_std_handle(int which);

static long WINAPI WindowsFDReader(Win_FD_Input_Thread *th);
static void WindowsFDICleanup(Win_FD_Input_Thread *th, int close_mode);

static long WINAPI WindowsFDWriter(Win_FD_Output_Thread *oth);
static void WindowsFDOCleanup(Win_FD_Output_Thread *oth, int close_mode);

typedef BOOL (WINAPI* CSI_proc)(HANDLE);

static CSI_proc get_csi(void)
{
  static int tried_csi = 0;
  static CSI_proc csi;
  
  if (!tried_csi) {
    HMODULE hm;
    hm = GetModuleHandleW(L"kernel32.dll");
    if (hm)
      csi = (CSI_proc)GetProcAddress(hm, "CancelSynchronousIo");
    else
      csi = NULL;
    tried_csi = 1;
  }
  return csi;
}

static intptr_t adjust_input_text(rktio_fd_t *rfd, char *buffer, char *is_converted,
				  intptr_t got, intptr_t offset);
static intptr_t adjust_input_text_for_pending_cr(rktio_fd_t *rfd, char *buffer, char *is_converted,
						 intptr_t got);
static const char *adjust_output_text(const char *buffer, intptr_t *towrite);
static intptr_t recount_output_text(const char *orig_buffer, const char *buffer, intptr_t wrote);

static wchar_t *convert_output_wtext(const char *buffer, intptr_t *_towrite,
				     int *_can_leftover, int *_keep_leftover,
				     int leftover_len, char *leftover,
				     int max_convert);
static intptr_t recount_output_wtext(wchar_t *w_buffer, intptr_t winwrote);

#define MIN_VIA_WIDE_BUFFER_SIZE 6
static int read_console_via_wide(HANDLE fd,
                                 wchar_t *w_buffer, DWORD w_buf_len, int *_w_buf_count,
                                 void *dest_buffer, DWORD dest_len, DWORD *_got);

#endif

static void wrote_to_terminal(intptr_t amt);

/*========================================================================*/
/* returning a result                                                     */
/*========================================================================*/

static rktio_result_t *maybe_success(intptr_t r, rktio_result_t *res, int err_val)
{
  if (r != err_val) {
    res->is_success = 1;
    res->success.i = r;
  } else
    res->is_success = 0;

  return res;
}

/*========================================================================*/
/* creating an fd                                                         */
/*========================================================================*/

/* Internally, we use this function as no-error atomic  */
rktio_fd_t *rktio_system_fd(rktio_t *rktio, intptr_t system_fd, int modes)
{
  rktio_fd_t *rfd;

  rfd = calloc(1, sizeof(rktio_fd_t));
  rfd->modes = (modes - (modes & RKTIO_OPEN_INIT));

#ifdef RKTIO_SYSTEM_UNIX
  rfd->fd = system_fd;
  if (!(modes & (RKTIO_OPEN_REGFILE | RKTIO_OPEN_NOT_REGFILE | RKTIO_OPEN_SOCKET))) {
    struct stat buf;
    int cr;
    do {
      cr = fstat(rfd->fd, &buf);
    } while ((cr == -1) && (errno == EINTR));
   if (S_ISREG(buf.st_mode))
      rfd->modes |= RKTIO_OPEN_REGFILE;
   if (!(modes & (RKTIO_OPEN_DIR | RKTIO_OPEN_NOT_DIR)))
     if (S_ISDIR(buf.st_mode))
       rfd->modes |= RKTIO_OPEN_DIR;
   if (!(rfd->modes & RKTIO_OPEN_REGFILE)) {
     if (rktio_system_fd_is_terminal(rktio, system_fd))
       rfd->modes |= RKTIO_OPEN_TRACK_TERMINAL_OUTPUT;
   }
  }
#endif

#ifdef RKTIO_SYSTEM_WINDOWS
  if (modes & RKTIO_OPEN_SOCKET)
    rfd->sock = (SOCKET)system_fd;
  else
    rfd->fd = (HANDLE)system_fd;
  if (!(modes & (RKTIO_OPEN_REGFILE | RKTIO_OPEN_NOT_REGFILE | RKTIO_OPEN_SOCKET))
      && (rfd->fd != DELAYED_CONSOLE_HANDLE)) {
    if ((GetFileType(rfd->fd) == FILE_TYPE_DISK))
      rfd->modes |= RKTIO_OPEN_REGFILE;
    if (!(modes & (RKTIO_OPEN_DIR | RKTIO_OPEN_NOT_DIR))) {
      BY_HANDLE_FILE_INFORMATION info;
      if (GetFileInformationByHandle(rfd->fd, &info)) {
        if (info.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) {
          rfd->modes |= RKTIO_OPEN_DIR;
        }
      }
    }
    if (!(rfd->modes & RKTIO_OPEN_REGFILE)) {
      if (rktio_system_fd_is_terminal(rktio, system_fd))
        rfd->modes |= RKTIO_OPEN_TRACK_TERMINAL_OUTPUT;
    }
  }
#endif
  
  if ((modes & RKTIO_OPEN_SOCKET) && (modes & RKTIO_OPEN_INIT))
    rktio_socket_init(rktio, rfd);
  
  if ((modes & RKTIO_OPEN_SOCKET) && (modes & RKTIO_OPEN_OWN))
    rktio_socket_own(rktio, rfd);

  return rfd;
}

int rktio_fd_is_pending_open(rktio_t *rktio, rktio_fd_t *rfd)
{
#ifdef RKTIO_USE_PENDING_OPEN
  if (rfd->pending)
    rktio_pending_open_poll(rktio, rfd, rfd->pending, &rktio->err);
  return !!rfd->pending;
#else
  return 0;
#endif
}

#ifdef RKTIO_USE_PENDING_OPEN
rktio_fd_t *rktio_pending_system_fd(rktio_t *rktio, struct open_in_thread_t *oit, int modes)
{
  rktio_fd_t *rfd;

  rfd = calloc(1, sizeof(rktio_fd_t));
  rfd->modes = (modes - (modes & RKTIO_OPEN_INIT));
  rfd->pending = oit;

  return rfd;
}

void rktio_update_system_fd(rktio_t *rktio, rktio_fd_t *rfd, int fd, int modes)
/* Convert an open-pending rfd to a normal rfd. The given `modes` adds
   to (and completes) the modes provided to `rktio_pending_system_fd`. */
{
  rfd->fd = fd;
  rfd->modes |= modes;
  rfd->pending = NULL;
}
#endif


intptr_t rktio_internal_fd_system_fd(rktio_fd_t *rfd)
{
#ifdef RKTIO_SYSTEM_UNIX
# ifdef RKTIO_USE_PENDING_OPEN
  if (rfd->pending) return -1;
# endif
  return rfd->fd;
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  if (rfd->modes & RKTIO_OPEN_SOCKET)
    return (intptr_t)rfd->sock;
  else {
    force_console(rfd);
    return (intptr_t)rfd->fd;
  }
#endif
}

intptr_t rktio_fd_system_fd(rktio_t *rktio, rktio_fd_t *rfd)
{
  return rktio_internal_fd_system_fd(rfd);
}

rktio_fd_t *rktio_std_fd(rktio_t *rktio, int which)
{
  int mode = ((which == RKTIO_STDIN)
              ? RKTIO_OPEN_READ
              : RKTIO_OPEN_WRITE);
#ifdef RKTIO_SYSTEM_UNIX
  return rktio_system_fd(rktio, which, mode | RKTIO_OPEN_NOT_DIR);
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  HANDLE h;
  switch (which) {
  case RKTIO_STDIN:
    which = STD_INPUT_HANDLE;
    break;
  case RKTIO_STDOUT:
    which = STD_OUTPUT_HANDLE;
    break;
  case RKTIO_STDERR:
    which = STD_ERROR_HANDLE;
    break;
  }
  h = get_std_handle(which);
  if ((h == INVALID_HANDLE_VALUE) || (h == NULL)) {
    h = DELAYED_CONSOLE_HANDLE; /* => open a console on demand */
  }
  return rktio_system_fd(rktio,
			 (intptr_t)h,
			 mode | RKTIO_OPEN_NOT_DIR);
#endif
}

int rktio_fd_is_regular_file(rktio_t *rktio, rktio_fd_t *rfd)
{
  return ((rfd->modes & RKTIO_OPEN_REGFILE) ? 1 : 0);
}

int rktio_fd_is_directory(rktio_t *rktio, rktio_fd_t *rfd)
{
  return ((rfd->modes & RKTIO_OPEN_DIR) ? 1 : 0);
}

int rktio_fd_is_socket(rktio_t *rktio, rktio_fd_t *rfd)
{
  return ((rfd->modes & RKTIO_OPEN_SOCKET) ? 1 : 0);
}

int rktio_fd_is_udp(rktio_t *rktio, rktio_fd_t *rfd)
{
  return ((rfd->modes & RKTIO_OPEN_UDP) ? 1 : 0);
}

int rktio_fd_is_open_pending(rktio_t *rktio, rktio_fd_t *rfd)
{
#ifdef RKTIO_USE_PENDING_OPEN
  if (rfd->pending) return 1;
#endif
  return 0;
}

int rktio_fd_modes(rktio_t *rktio, rktio_fd_t *rfd)
{
  return rfd->modes;
}

int rktio_system_fd_is_terminal(rktio_t *rktio, intptr_t fd)
{
#ifdef RKTIO_SYSTEM_UNIX
  return isatty(fd);
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  if ((HANDLE)fd == DELAYED_CONSOLE_HANDLE)
    return 1; /* delayed console */
  else if (GetFileType((HANDLE)fd) == FILE_TYPE_CHAR) {
    DWORD mode;
    if (GetConsoleMode((HANDLE)fd, &mode))
      return 1;
    else
      return 0;
  } else
    return 0;
#endif
}

int rktio_fd_is_terminal(rktio_t *rktio, rktio_fd_t *rfd)
{
#ifdef RKTIO_USE_PENDING_OPEN
  if (rfd->pending) return 0;
#endif
  if (rfd->modes & RKTIO_OPEN_TRACK_TERMINAL_OUTPUT) return 1;
  return rktio_system_fd_is_terminal(rktio, (intptr_t)rfd->fd);
}

int rktio_fd_is_text_converted(rktio_t *rktio, rktio_fd_t *rfd)
{
#ifdef RKTIO_SYSTEM_WINDOWS
  if (rfd->modes & RKTIO_OPEN_TEXT)
    return 1;
#endif
  return 0;
}

rktio_fd_t *rktio_dup(rktio_t *rktio, rktio_fd_t *rfd)
{
#ifdef RKTIO_SYSTEM_UNIX
  intptr_t nfd;

# ifdef RKTIO_USE_PENDING_OPEN
  if (rfd->pending) {
    rktio_pending_open_retain(rktio, rfd->pending);
    return rktio_pending_system_fd(rktio, rfd->pending, rfd->modes);
  }
# endif

  rktio_cloexec_lock();

  do {
    nfd = dup(rfd->fd);
  } while (nfd == -1 && errno == EINTR);

  if (nfd != -1)
    rktio_fd_cloexec(nfd);
  else
    get_posix_error();

  rktio_cloexec_unlock();

  if (nfd == -1) {
    return NULL;
  } else {
    /* Set the `RKTIO_OPEN_INIT` flag, because dup()ing a file
       descriptor does not keep all of its properties on some
       platforms (e.g., the non-blocking property on sockets on
       Linux). */
    return rktio_system_fd(rktio, nfd, rfd->modes | RKTIO_OPEN_INIT);
  }
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  if (rfd->modes & RKTIO_OPEN_SOCKET) {
    return rktio_socket_dup(rktio, rfd);
  } else {
    HANDLE  newhandle;
    BOOL rc;

    if (rfd->fd == DELAYED_CONSOLE_HANDLE) {
      newhandle = DELAYED_CONSOLE_HANDLE;
      rc = TRUE;
    } else
      rc = DuplicateHandle(GetCurrentProcess(), rfd->fd,
			   GetCurrentProcess(), &newhandle,
			   0, FALSE, DUPLICATE_SAME_ACCESS);

    if (rc == FALSE) {
      get_windows_error();
      return NULL;
    } else {
      return rktio_system_fd(rktio, (intptr_t)newhandle, rfd->modes);
    }
  }
#endif
}


void rktio_fd_cloexec(intptr_t fd) {
#ifdef RKTIO_SYSTEM_UNIX
# ifdef RKTIO_HAS_CLOEXEC
  int flags;
  flags = fcntl(fd, F_GETFD, 0);
  fcntl(fd, F_SETFD, flags | FD_CLOEXEC);
# endif
#endif
}

/*========================================================================*/
/* closing                                                                */
/*========================================================================*/

#ifdef RKTIO_SYSTEM_UNIX
int rktio_reliably_close_err(intptr_t s) {
  int cr;
  do { 
    cr = close(s);
  } while ((cr == -1) && (errno == EINTR));
  return cr;
}

void rktio_reliably_close(intptr_t s)
{
  (void)rktio_reliably_close_err(s);
}
#endif

static rktio_ok_t do_close(rktio_t *rktio /* maybe NULL */, rktio_fd_t *rfd, int set_error)
/* The `rktio` argument can be NULL for a detached file descriptor */
{
  int ok;

#ifdef RKTIO_SYSTEM_UNIX
  int cr;
#endif
#ifdef RKTIO_USE_IO_URING
  int ring_closes = 0;
  if (rfd->u)
    ring_closes = uring_release(rktio, rfd, 1);
#endif

#ifdef RKTIO_SYSTEM_UNIX

# ifdef RKTIO_USE_PENDING_OPEN
  if (rfd->pending)
    cr = rktio_pending_open_release(rktio, rfd->pending);
  else
# endif
# ifdef RKTIO_USE_IO_URING
  if (ring_closes)
    cr = 0;
  else
# endif
    cr = rktio_reliably_close_err(rfd->fd);

# ifdef RKTIO_USE_FCNTL_AND_FORK_FOR_FILE_LOCKS
  if (rktio && !(rfd->modes & RKTIO_OPEN_SOCKET) && (!cr || !set_error))
    rktio_release_lockf(rktio, rfd->fd);
# endif

  if (cr && set_error) {
    get_posix_error();   
    ok = 0;
  } else
    ok = 1;
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  if (rfd->modes & RKTIO_OPEN_SOCKET)
    return rktio_socket_close(rktio, rfd, set_error);

  deinit_fd(rfd, 1);

  ok = 1;
  if (!rfd->th && !rfd->oth && (rfd->fd != DELAYED_CONSOLE_HANDLE)) {
    if (!CloseHandle(rfd->fd)) {
      ok = 0;
      if (set_error)
        get_windows_error();
    }
  }
  
#endif

  if (ok || !set_error)
    free(rfd);

  return ok;
}

rktio_ok_t rktio_close(rktio_t *rktio, rktio_fd_t *rfd)
{
  return do_close(rktio, rfd, 1);
}

void rktio_close_noerr(rktio_t *rktio, rktio_fd_t *rfd)
{
  (void)do_close(rktio, rfd, 0);
}

rktio_ok_t rktio_internal_close(rktio_t *rktio /* may be NULL */, rktio_fd_t *rfd, int set_error)
{
  return do_close(rktio, rfd, set_error);
}

void rktio_forget(rktio_t *rktio, rktio_fd_t *rfd)
{
#ifdef RKTIO_USE_IO_URING
  if (rfd->u)
    uring_release(rktio, rfd, 0);
#endif
#ifdef RKTIO_WINDOWS_SYSTEM
  deinit_fd(rfd, 1);
#endif
  free(rfd);
}

/*========================================================================*/
/* detach, attach, and abandon                                            */
/*========================================================================*/

rktio_fd_transfer_t *rktio_fd_detach(rktio_t *rktio, rktio_fd_t *rfd)
{
#ifdef RKTIO_USE_IO_URING
  /* prototype: any data buffered by io_uring is lost */
  if (rfd->u)
    uring_release(rktio, rfd, 0);
#endif
#ifdef RKTIO_USE_PENDING_OPEN
  if (rfd->pending)
    rktio_pending_open_detach(rktio, rfd->pending);
#endif

  return (rktio_fd_transfer_t *)rfd;
}

rktio_fd_t *rktio_fd_attach(rktio_t *rktio, rktio_fd_transfer_t *rfdt)
{
  rktio_fd_t *rfd = (rktio_fd_t *)rfdt;

#ifdef RKTIO_USE_PENDING_OPEN
  if (rfd->pending)
    rktio_pending_open_attach(rktio, rfd->pending);
#endif

  return rfd;
}

void rktio_fd_close_transfer(rktio_fd_transfer_t *rfdt)
{
  (void)do_close(NULL, (rktio_fd_t *)rfdt, 0);
}

/*========================================================================*/
/* polling                                                                */
/*========================================================================*/

#ifdef SOME_FDS_ARE_NOT_SELECTABLE
static int try_get_fd_char(int fd, int *ready)
{
  int old_flags, c;
  unsigned char buf[1];

  old_flags = fcntl(fd, F_GETFL, 0);
  if (!(old_flags & RKTIO_NONBLOCKING))
    fcntl(fd, F_SETFL, old_flags | RKTIO_NONBLOCKING);
  do {
    c = read(fd, buf, 1);
  } while ((c == -1) && errno == EINTR);
  if (!(old_flags & RKTIO_NONBLOCKING))
    fcntl(fd, F_SETFL, old_flags);

  if (c < 0) {
    *ready = 0;
    return 0;
  } else {
    *ready = 1;
    if (!c)
      return RKTIO_READ_EOF;
    else
      return buf[0];
  }
}
#endif

static int do_poll_read_ready(rktio_t *rktio, rktio_fd_t *rfd, rktio_err_t *err)
{
  if (rktio_fd_is_regular_file(rktio, rfd))
    return RKTIO_POLL_READY;

#ifdef RKTIO_USE_IO_URING
  if (rfd->u)
    return uring_poll_read_ready(rktio, rfd);
#endif

#ifdef RKTIO_SYSTEM_UNIX
  {
    int r;

# ifdef SOME_FDS_ARE_NOT_SELECTABLE
    if (rfd->bufcount)
      return RKTIO_POLL_READY;
# endif

# ifdef HAVE_POLL_SYSCALL
    struct pollfd pfd[1];
    pfd[0].fd = rfd->fd;
    pfd[0].events = POLLIN;
    do {
      r = poll(pfd, 1, 0);
    } while ((r == -1) && (errno == EINTR));
# else
    DECL_FDSET(readfds, 1);
    DECL_FDSET(exnfds, 1);
    struct timeval time = {0, 0};

    INIT_DECL_RD_FDSET(readfds);
    INIT_DECL_ER_FDSET(exnfds);

    RKTIO_FD_ZERO(readfds);
    RKTIO_FD_ZERO(exnfds);
    RKTIO_FD_SET(rfd->fd, readfds);
    RKTIO_FD_SET(rfd->fd, exnfds);

    do {
      r = select(rfd->fd + 1, RKTIO_FDS(readfds), NULL, RKTIO_FDS(exnfds), &time);
    } while ((r == -1) && (errno == EINTR));
# endif

# ifdef SOME_FDS_ARE_NOT_SELECTABLE
    /* Try a non-blocking read: */
    if (!r && !(rfd->modes & RKTIO_OPEN_SOCKET)) {
      int c, ready;

      c = try_get_fd_char(rfd->fd, &ready);
      if (ready) {
        if (c != RKTIO_READ_EOF) {
          rfd->buffer[0] = (unsigned char)c;
          rfd->bufcount = 1;
        }
        r = 1;
      }
    }
# endif

    return r != 0;
  }
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  if (rfd->modes & RKTIO_OPEN_SOCKET)
    return rktio_socket_poll_read_ready(rktio, rfd, err);

  init_read_fd(rktio, rfd);

  if (rfd->leftover_len)
    return RKTIO_POLL_READY;

  if (!rfd->th) {
    /* No thread, so use wait. See `USE_BACKGROUND_THREAD_FOR_CONSOLE_INPUT`
       for why this isn't ready right for console handles. */
    if (WaitForSingleObject(rfd->fd, 0) == WAIT_OBJECT_0)
      return RKTIO_POLL_READY;
  } else {
    /* Has the reader thread pulled in data? */
    WaitForSingleObject(rfd->th->lock_sema, INFINITE);
    if (rfd->th->checking) {
      /* The thread is still trying.
	 Clean up any signals that we may have ignored before. */
      WaitForSingleObject(rfd->th->ready_sema, 0);
    } else if (rfd->th->avail || rfd->th->err || rfd->th->eof) {
      ReleaseSemaphore(rfd->th->lock_sema, 1, NULL);
      return RKTIO_POLL_READY; /* other thread found data */
    } else {
      /* Doesn't have anything, and it's not even looking. Tell it
         to look: */
      rfd->th->checking = 1;
      ReleaseSemaphore(rfd->th->checking_sema, 1, NULL);
    }
    ReleaseSemaphore(rfd->th->lock_sema, 1, NULL);
  }

  return 0;
#endif
}

int rktio_poll_read_ready(rktio_t *rktio, rktio_fd_t *rfd)
{
  return do_poll_read_ready(rktio, rfd, &rktio->err);
}

rktio_result_t *rktio_poll_read_ready_r(rktio_t *rktio, rktio_fd_t *rfd)
{
  return maybe_success(do_poll_read_ready(rktio, rfd, &rfd->res.err), &rfd->res, RKTIO_POLL_ERROR);
}

static int poll_write_ready_or_flushed(rktio_t *rktio, rktio_fd_t *rfd, int check_flushed, rktio_err_t *err)
{
#ifdef RKTIO_USE_IO_URING
  if (rfd->u)
    return uring_poll_write_ready(rktio, rfd);
#endif
#ifdef RKTIO_SYSTEM_UNIX
  if (check_flushed)
    return RKTIO_POLL_READY;
  else {
    int sr;

# ifdef RKTIO_USE_PENDING_OPEN
    if (rfd->pending) {
      int errval = rktio_pending_open_poll(rktio, rfd, rfd->pending, err);
      if (errval) {
        errno = errval;
        rktio_get_posix_error(err);
        return RKTIO_POLL_ERROR;
      } else if (rfd->pending)
        return RKTIO_POLL_NOT_READY;
    }
# endif

# ifdef HAVE_POLL_SYSCALL
    struct pollfd pfd[1];
    pfd[0].fd = rfd->fd;
    pfd[0].events = POLLOUT;
    do {
      sr = poll(pfd, 1, 0);
    } while ((sr == -1) && (errno == EINTR));
# else
    DECL_FDSET(writefds, 1);
    DECL_FDSET(exnfds, 1);
    struct timeval time = {0, 0};
  
    INIT_DECL_WR_FDSET(writefds);
    INIT_DECL_ER_FDSET(exnfds);
  
    RKTIO_FD_ZERO(writefds);
    RKTIO_FD_ZERO(exnfds);
    RKTIO_FD_SET(rfd->fd, writefds);
    RKTIO_FD_SET(rfd->fd, exnfds);
  
    do {
      /* Mac OS X 10.8 and 10.9: select() seems to claim that a pipe
         is always ready for output. To work around that problem,
         kqueue() support might be used for pipes, but that has different
         problems. The pol() code above should be used, instead. */
      sr = select(rfd->fd + 1, NULL, RKTIO_FDS(writefds), RKTIO_FDS(exnfds), &time);
    } while ((sr == -1) && (errno == EINTR));
# endif

    if (sr == -1) {
      rktio_get_posix_error(err);
      return RKTIO_POLL_ERROR;
    } else
      return (sr != 0);
  }
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  if (rfd->fd == DELAYED_CONSOLE_HANDLE)
    return RKTIO_POLL_READY;

  if (rfd->modes & RKTIO_OPEN_SOCKET) {
    if (check_flushed)
      return RKTIO_POLL_READY;
    return rktio_socket_poll_write_ready(rktio, rfd, err);
  }
  
  if (rfd->oth) {
    /* Pipe output that can block or needs a background flush */
    int retval;
    Win_FD_Output_Thread *oth = rfd->oth;

    if (check_flushed && rfd->oth->nonblocking) {
      /* Not Windows 95, so any written data really is in the pipe, as
         good as flushed, and we don't really need to ask the thread. */
      return RKTIO_POLL_READY;
    }

    WaitForSingleObject(oth->lock_sema, INFINITE);
    if (oth->nonblocking) {
      if (oth->needflush) {
	oth->needflush = 0;
	oth->flushed = 0;
	ReleaseSemaphore(oth->work_sema, 1, NULL); /* start trying to flush */
	retval = 0;
      } else
	retval = oth->flushed;
      if (!retval) {
	/* While we hold the lock, clear any leftover notifications */
	WaitForSingleObject(oth->ready_sema, 0);
      }
    } else {
      /* Using separate writing thread for Windows 95 */
      if (oth->err_no) {
	/* Delay error report until next write */
	retval = 1;
      } else
	retval = (check_flushed
		  ? !oth->buflen
		  : (oth->buflen < RKTIO_FD_BUFFSIZE));
      if (!retval && !check_flushed) {
	/* clear any leftover notifications */
	WaitForSingleObject(oth->ready_sema, 0);
      }
    }
    ReleaseSemaphore(oth->lock_sema, 1, NULL);

    return (retval ? RKTIO_POLL_READY : 0);
  } else
    return RKTIO_POLL_READY; /* non-blocking output, such as a console, or haven't written yet */
#endif
}

int rktio_poll_write_ready(rktio_t *rktio, rktio_fd_t *rfd)
{
  return poll_write_ready_or_flushed(rktio, rfd, 0, &rktio->err);
}

int rktio_poll_write_flushed(rktio_t *rktio, rktio_fd_t *rfd)
{
  return poll_write_ready_or_flushed(rktio, rfd, 1, &rktio->err);
}

rktio_result_t *rktio_poll_write_ready_r(rktio_t *rktio, rktio_fd_t *rfd)
{
  return maybe_success(poll_write_ready_or_flushed(rktio, rfd, 0, &rfd->res.err), &rfd->res, RKTIO_POLL_ERROR);
}

rktio_result_t *rktio_poll_write_flushed_r(rktio_t *rktio, rktio_fd_t *rfd)
{
  return maybe_success(poll_write_ready_or_flushed(rktio, rfd, 1, &rfd->res.err), &rfd->res, RKTIO_POLL_ERROR);
}

rktio_result_t *rktio_poll_read_ready_r(rktio_t *rktio, rktio_fd_t *rfd);
RKTIO_EXTERN_RESULT(rktio_result_integer)
rktio_result_t *rktio_poll_write_ready_r(rktio_t *rktio, rktio_fd_t *rfd);


void rktio_poll_add(rktio_t *rktio, rktio_fd_t *rfd, rktio_poll_set_t *fds, int modes)
{
#ifdef RKTIO_SYSTEM_UNIX
  rktio_poll_set_t *fds2;

# ifdef RKTIO_USE_IO_URING
  if (rfd->u) {
    /* completions make the ring's fd readable */
    if (modes & RKTIO_POLL_READ)
      (void)uring_poll_read_ready(rktio, rfd); /* make sure a recv is queued */
    RKTIO_FD_SET(rktio->uring->fd, fds);
    return;
  }
# endif

# ifdef RKTIO_USE_PENDING_OPEN
  if (rfd->pending) {
    rktio_poll_add_pending_open(rktio, rfd, rfd->pending, fds);
    return;
  }
# endif

  if (modes & RKTIO_POLL_READ) {
    RKTIO_FD_SET(rfd->fd, fds);
  }
  if (modes & RKTIO_POLL_WRITE) {
    fds2 = RKTIO_GET_FDSET(fds, 1);
    RKTIO_FD_SET(rfd->fd, fds2);
  }
  fds2 = RKTIO_GET_FDSET(fds, 2);
  RKTIO_FD_SET(rfd->fd, fds2);
#endif  
#ifdef RKTIO_SYSTEM_WINDOWS
  if (rfd->modes & RKTIO_OPEN_SOCKET) {
    /* RKTIO_FD_SET(), etc., for Windows expects sockets */
    rktio_poll_set_t *fds2;

    if (modes & RKTIO_POLL_READ) {
      RKTIO_FD_SET((intptr_t)rfd->sock, fds);
    }
    if (modes & RKTIO_POLL_WRITE) {
      fds2 = RKTIO_GET_FDSET(fds, 1);
      RKTIO_FD_SET((intptr_t)rfd->sock, fds2);
    }
    fds2 = RKTIO_GET_FDSET(fds, 2);
    RKTIO_FD_SET((intptr_t)rfd->sock, fds2);
  } else {
    force_console(rfd);

    if (modes & RKTIO_POLL_READ) {
      init_read_fd(rktio, rfd);
      if (rfd->th) {
	WaitForSingleObject(rfd->th->lock_sema, INFINITE);
        if (!rfd->th->checking) {
          if (rfd->th->avail || rfd->th->err || rfd->th->eof) {
            /* Data is ready. We shouldn't be trying to sleep, so force an
               immediate wake-up: */
	    ReleaseSemaphore(rfd->th->lock_sema, 1, NULL);
            rktio_poll_set_add_nosleep(rktio, fds);
          } else {
	    /* Ask the reader thread to start checking for data */
            rfd->th->checking = 1;
            ReleaseSemaphore(rfd->th->checking_sema, 1, NULL);
	    /* clear any notifications that we may have ignored before */
	    WaitForSingleObject(rfd->th->ready_sema, 0);
	    ReleaseSemaphore(rfd->th->lock_sema, 1, NULL);
            rktio_poll_set_add_handle(rktio, (intptr_t)rfd->th->ready_sema, fds, 1);
          }
        } else {
	  ReleaseSemaphore(rfd->th->lock_sema, 1, NULL);
          rktio_poll_set_add_handle(rktio, (intptr_t)rfd->th->ready_sema, fds, 1);
	}
      } else if (rktio_fd_is_regular_file(rktio, rfd)) {
        /* regular files never block */
        rktio_poll_set_add_nosleep(rktio, fds);
      } else {
        /* We get here for console input with
	   USE_BACKGROUND_THREAD_FOR_CONSOLE_INPUT is 0, for
	   example.  */
	if (rfd->leftover_len)
	  rktio_poll_set_add_nosleep(rktio, fds);
	else {
	  /* This  is not really right for consoles.
	     See `USE_BACKGROUND_THREAD_FOR_CONSOLE_INPUT`. */
	  rktio_poll_set_add_handle(rktio, (intptr_t)rfd->fd, fds, 0);
	}
      }
    }

    if (modes & RKTIO_POLL_WRITE) {
      if (rfd->oth && !rktio_poll_write_ready(rktio, rfd))
        rktio_poll_set_add_handle(rktio, (intptr_t)rfd->oth->ready_sema, fds, 1);
      else
        rktio_poll_set_add_nosleep(rktio, fds);
    }

    if (modes & RKTIO_POLL_FLUSH) {
      if (rfd->oth && !rktio_poll_write_flushed(rktio, rfd))
        rktio_poll_set_add_handle(rktio, (intptr_t)rfd->oth->ready_sema, fds, 1);
      else
        rktio_poll_set_add_nosleep(rktio, fds);
    }
  }
#endif
}

/*========================================================================*/
/* reading                                                                */
/*========================================================================*/

static intptr_t do_read_converted(rktio_t *rktio, rktio_fd_t *rfd, char *buffer, intptr_t len,
                                  char *is_converted, rktio_err_t *err)
{
#ifdef RKTIO_SYSTEM_UNIX
  intptr_t bc;

# ifdef RKTIO_USE_IO_URING
  if (rfd->u)
    return uring_read(rktio, rfd, buffer, len, err);
# endif

  if (rfd->modes & RKTIO_OPEN_SOCKET)
    return rktio_socket_read(rktio, rfd, buffer, len, err);

# ifdef SOME_FDS_ARE_NOT_SELECTABLE
  if (rfd->bufcount && len) {
    buffer[0] = rfd->buffer[0];
    rfd->bufcount = 0;
    return 1;
  }
# endif

  len = LIMIT_REQUEST_SIZE(len);

  if (rktio_fd_is_regular_file(rktio, rfd)) {
    /* Reading regular file never blocks */
    do {
      bc = read(rfd->fd, buffer, len);
    } while ((bc == -1) && (errno == EINTR));

    if (bc == -1) {
      rktio_get_posix_error(err);
      return RKTIO_READ_ERROR;
    } else if (bc == 0)
      return RKTIO_READ_EOF;
    else
      return bc;
  } else {
    int old_flags;

    /* Check that input is available. This step is needed
       for fifos to avoid getting an EOF-looking result
       if there's no writer, yet. */
    if (!rktio_poll_read_ready(rktio, rfd))
      return 0;
    
    old_flags = fcntl(rfd->fd, F_GETFL, 0);
    if (!(old_flags & RKTIO_NONBLOCKING))
      fcntl(rfd->fd, F_SETFL, old_flags | RKTIO_NONBLOCKING);
    
    /* We use a non-blocking read here, even though we've waited
       for input above, because an external process might have
       gobbled the characters that we expected to get. */
    do {
      bc = read(rfd->fd, buffer, len);
    } while ((bc == -1) && errno == EINTR);

    if ((bc == -1) && (errno != EAGAIN))
      rktio_get_posix_error(err);
    
    if (!(old_flags & RKTIO_NONBLOCKING))
      fcntl(rfd->fd, F_SETFL, old_flags);
    
    if (bc == -1) {
      if (errno == EAGAIN)
        return 0; /* no bytes from a non-blocking read */
      else
        return RKTIO_READ_ERROR;
    } else if (bc == 0)
      return RKTIO_READ_EOF;
    else
      return bc;
  }
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  if (rfd->modes & RKTIO_OPEN_SOCKET)
    return rktio_socket_read(rktio, rfd, buffer, len, err);

  init_read_fd(rktio, rfd);

  if (!rfd->th) {
    /* We can read directly. This must be a regular file, where
       reading never blocks, or it's a console, where we can check. */
    DWORD rgot, offset = 0;
    int ready;

    if (rktio_fd_is_regular_file(rktio, rfd))
      ready = -1; /* => regfile */
    else {
      if (rfd->leftover_len)
        ready = 1;
      else {
	/* See `USE_BACKGROUND_THREAD_FOR_CONSOLE_INPUT` for why this
	   isn't ready right for console handles. */
	ready = (WaitForSingleObject((HANDLE)rfd->fd, 0) == WAIT_OBJECT_0);
      }
    }

    if (rfd->has_pending_byte) {
      if (!len)
        return 0;
      buffer[0] = rfd->pending_byte;
      if (len == 1) {
        if (rfd->pending_byte == '\r') {
          /* We have to read one more byte and then decode,
             shifting the new byte into pending position
             if it's not '\n' */
          if (!ready) return 0;
        } else {
          if (is_converted) is_converted[0] = 0;
          rfd->has_pending_byte = 0;
          return 1;
        }
      } else {
        if (!ready) return 0;
        /* read after first byte installed into the buffer */
        rfd->has_pending_byte = 0;
        offset = 1;
      }
    }

    if (ready < 0)
      ready = ReadFile((HANDLE)rfd->fd, buffer + offset, len - offset, &rgot, NULL);
    else {
      DWORD amt = len - offset;
      if (rfd->leftover_len) {
        if (amt > rfd->leftover_len)
          rgot = rfd->leftover_len;
        else
          rgot = amt;
        memcpy(buffer + offset, rfd->leftover, rgot);
        rfd->leftover_len -= rgot;
        if (rfd->leftover_len)
          memmove(rfd->leftover, rfd->leftover + rgot, rfd->leftover_len);
        ready = 1;
      } else if (amt >= MIN_VIA_WIDE_BUFFER_SIZE) {
        /* limit reading to the max that definitely fits in `amt` */
        DWORD w_fill_len = amt / MIN_VIA_WIDE_BUFFER_SIZE;
        if (w_fill_len > (RKTIO_FD_BUFFSIZE / sizeof(wchar_t))) {
          /* at the same time, we mus not overflow the wide-character buffer */
          w_fill_len = (RKTIO_FD_BUFFSIZE / sizeof(wchar_t));
        }
        ready = read_console_via_wide(rfd->fd,
                                      (wchar_t *)rfd->buffer, w_fill_len, &rfd->w_buf_count,
                                      buffer + offset, len - offset, &rgot);
      } else {
        /* we have to read into a buffer that is definitely large enough to decode
           one wide character; they may be leftover bytes if `amt` isn't big enough
           for the decoding */
        ready = read_console_via_wide(rfd->fd,
                                      (wchar_t *)rfd->buffer, 1, &rfd->w_buf_count,
                                      rfd->leftover, MIN_VIA_WIDE_BUFFER_SIZE, &rgot);
        if (ready && rgot) {
          if (amt > rgot) amt = rgot;
          memcpy(buffer, rfd->leftover, amt);
          rfd->leftover_len = rgot - amt;
          memmove(rfd->leftover, rfd->leftover + amt, rfd->leftover_len);
          rgot = amt;
        }
      }
    }

    if (!ready) {
      rktio_get_windows_error(err);
      return RKTIO_READ_ERROR;
    }

    rgot += offset;
    
    if (rfd->has_pending_byte) {
      /* We had a buffer of size 1 and a pending '\r'... */
      return adjust_input_text_for_pending_cr(rfd, buffer, is_converted, rgot);
    } else if (!rgot)
      return RKTIO_READ_EOF;
    else if (rfd->modes & RKTIO_OPEN_TEXT)
      return adjust_input_text(rfd, buffer, is_converted, rgot, offset);
    else
      return rgot;
  } else {
    if (!rktio_poll_read_ready(rktio, rfd))
      return 0;
    
    /* If we get this far, there should be data available.
       Extract data made available by the reader thread. */
    WaitForSingleObject(rfd->th->lock_sema, INFINITE);
    if (rfd->th->eof) {
      if (rfd->th->eof != INVALID_HANDLE_VALUE) {
        ReleaseSemaphore(rfd->th->eof, 1, NULL);
        rfd->th->eof = NULL;
      }
      ReleaseSemaphore(rfd->th->lock_sema, 1, NULL);
      return RKTIO_READ_EOF;
    } else if (rfd->th->err) {
      ReleaseSemaphore(rfd->th->lock_sema, 1, NULL);
      rktio_set_windows_error(err, rfd->th->err);
      return RKTIO_READ_ERROR;
    } else {
      intptr_t bc = rfd->th->avail;
      if (bc > len)
	bc = len;
      rfd->th->avail -= bc;
      memcpy(buffer, rfd->buffer + rfd->th->offset, bc);
      rfd->th->offset += bc;
      ReleaseSemaphore(rfd->th->lock_sema, 1, NULL);
      return bc;
    }
  }
#endif
}

intptr_t rktio_read_converted(rktio_t *rktio, rktio_fd_t *rfd, char *buffer, intptr_t len,
                              char *is_converted)
{
  return do_read_converted(rktio, rfd, buffer, len, is_converted, &rktio->err);
}

intptr_t rktio_read_converted_in(rktio_t *rktio, rktio_fd_t *rfd, char *buffer, intptr_t start, intptr_t end,
                                 char *is_converted, intptr_t converted_start)
{
  return do_read_converted(rktio, rfd, buffer+start, end-start, is_converted+converted_start, &rktio->err);
}

rktio_result_t *rktio_read_converted_in_r(rktio_t *rktio, rktio_fd_t *rfd, char *buffer, intptr_t start, intptr_t end,
                                          char *is_converted, intptr_t converted_start)
{
  return maybe_success(do_read_converted(rktio, rfd, buffer+start, end-start, is_converted+converted_start,
                                         &rfd->res.err),
                       &rfd->res,
                       RKTIO_READ_ERROR);
}

intptr_t rktio_read(rktio_t *rktio, rktio_fd_t *rfd, char *buffer, intptr_t len)
{
  return rktio_read_converted(rktio, rfd, buffer, len, NULL);
}

intptr_t rktio_read_in(rktio_t *rktio, rktio_fd_t *rfd, char *buffer, intptr_t start, intptr_t end)
{
  return rktio_read_converted(rktio, rfd, buffer+start, end-start, NULL);
}

rktio_result_t *rktio_read_in_r(rktio_t *rktio, rktio_fd_t *rfd, char *buffer, intptr_t start, intptr_t end)
{
  return maybe_success(do_read_converted(rktio, rfd, buffer+start, end-start, NULL, &rfd->res.err),
                       &rfd->res,
                       RKTIO_READ_ERROR);
}

RKTIO_EXTERN intptr_t rktio_buffered_byte_count(rktio_t *rktio, rktio_fd_t *fd)
{
#ifdef RKTIO_SYSTEM_UNIX
# ifdef SOME_FDS_ARE_NOT_SELECTABLE
  return fd->bufcount;
# else
  return 0;
# endif
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  return ((fd->has_pending_byte ? 1 : 0) + fd->leftover_len);
#endif
}

#ifdef RKTIO_SYSTEM_WINDOWS

static intptr_t adjust_input_text(rktio_fd_t *rfd, char *buffer, char *is_converted,
				  intptr_t got, intptr_t offset)
{
  int i, j;

  if (got && (buffer[got-1] == '\r') && !offset) {
    /* Save '\r' that might be followed by '\n' */
    rfd->pending_byte = '\r';
    rfd->has_pending_byte = 1;
    --got;
  }
  
  for (i = 0, j = 0; i < got-1; i++) {
    if ((buffer[i] == '\r') && (buffer[i+1] == '\n')) {
      if (is_converted) is_converted[j] = 1;
      buffer[j++] = '\n';
      i++;
    } else {
      if (is_converted) is_converted[j] = 0;
      buffer[j++] = buffer[i];
    }
  }
  if (i < got) {
    if (is_converted) is_converted[j] = 0;
    buffer[j++] = buffer[i];
  }

  return j;
}

static intptr_t adjust_input_text_for_pending_cr(rktio_fd_t *rfd, char *buffer, char *is_converted,
						 intptr_t got)
{
  if (!got) {
    /* There are no more bytes to decode. Report the final '\r' by itself */
    buffer[0] = '\r';
    rfd->has_pending_byte = 0;
    if (is_converted) is_converted[0] = 0;
    return 1;
  } else {
    /* Combine the one new byte with a preceding '\r' */
    if (buffer[0] == '\n') {
      /* Decode */
      rfd->has_pending_byte = 0;
      if (is_converted) is_converted[0] = 1;
      return 1;
    } else {
      /* Save the new byte as pending while returning a '\r' by itself */
      rfd->pending_byte = buffer[0];
      buffer[0] = '\r';
      if (is_converted) is_converted[0] = 0;
      return 1;
    }
  }
}

static void init_read_fd(rktio_t *rktio, rktio_fd_t *rfd)
{
  force_console(rfd);

  if (rfd->th)
    return;
  else if (rktio_fd_is_regular_file(rktio, rfd)) {
    return;
  } else if (!USE_BACKGROUND_THREAD_FOR_CONSOLE_INPUT
	     && rktio_fd_is_terminal(rktio, rfd)) {
    if (!rfd->buffer) {
      char *bfr;
      bfr = malloc(RKTIO_FD_BUFFSIZE);
      rfd->buffer = bfr;
      rfd->w_buf_count = 0;
    }
    return;
  } else {
    /* To get non-blocking I/O for anything that can block, we create
       a separate reader thread.

       Yes, Windows NT pipes support non-blocking reads, but there
       doesn't seem to be any way to use WaitForSingleObject to sleep
       until characters are ready. PeekNamedPipe can be used for
       polling, but not sleeping. */

    Win_FD_Input_Thread *th;
    DWORD id;
    HANDLE h;
    HANDLE sm;
    char *bfr;

    th = calloc(1, sizeof(Win_FD_Input_Thread));
    rfd->th = th;

    bfr = malloc(RKTIO_FD_BUFFSIZE);
    th->buffer = (unsigned char *)bfr;
    rfd->buffer = bfr;

    th->fd = rfd->fd;
    th->avail = 0;
    th->offset = 0;
    th->err = 0;
    th->eof = NULL;
    th->checking = 0;
    
    sm = CreateSemaphore(NULL, 1, 1, NULL);
    th->lock_sema = sm;
    sm = CreateSemaphore(NULL, 0, 1, NULL);
    th->checking_sema = sm;
    sm = CreateSemaphore(NULL, 0, 1, NULL);
    th->ready_sema = sm;

    h = CreateThread(NULL, 4096, (LPTHREAD_START_ROUTINE)WindowsFDReader, th, 0, &id);

    th->thread = h;
  }
}

static void deinit_read_fd(rktio_fd_t *rfd, int full_close)
{
  if (rfd->th) {
    CSI_proc csi;

    WaitForSingleObject(rfd->th->lock_sema, INFINITE);
    
    /* -1 for checking means "shut down" */
    rfd->th->checking = -1;
    ReleaseSemaphore(rfd->th->checking_sema, 1, NULL);

    if (rfd->th->eof && (rfd->th->eof != INVALID_HANDLE_VALUE)) {
      ReleaseSemaphore(rfd->th->eof, 1, NULL);
      rfd->th->eof = NULL;
    }

    csi = get_csi();
    if (csi) {
      /* Helps thread wake up. Otherwise, it's possible for the
         thread to stay stuck trying to read, in which case the
         file handle (probably a pipe) doesn't get closed. */
      csi(rfd->th->thread);
    }

    /* Try to get out of cleaning up the records (since they can't be
       cleaned until the thread is also done: */
    if (rfd->th->you_clean_up) {
      /* The other thread exited and left us with clean-up: */
      WindowsFDICleanup(rfd->th, (full_close ? 1 : -1));
    } else {
      /* otherwise, thread is responsible for clean-up */
      rfd->th->you_clean_up = (full_close ? 1 : -1);
      ReleaseSemaphore(rfd->th->lock_sema, 1, NULL);
    }

    rfd->buffer = NULL;
  } else if (rfd->buffer) {
    free(rfd->buffer);
    rfd->buffer = NULL;
  }
}

static long WINAPI WindowsFDReader(Win_FD_Input_Thread *th)
{
  DWORD toget, got;
  int perma_eof = 0, ft, is_console = 0;
  HANDLE eof_wait = NULL;
# define CONSOLE_BUFFER_IN_SIZE 16
  wchar_t w_buffer[CONSOLE_BUFFER_IN_SIZE];
  int w_buf_count = 0;

  ft = GetFileType((HANDLE)th->fd);
    
  if (ft == FILE_TYPE_PIPE) {
    /* Reading from a pipe will return early when data is available. */
    toget = RKTIO_FD_BUFFSIZE;
  } else {
    /* Non-pipe: get one char at a time: */
    toget = 1;
    is_console = (ft == FILE_TYPE_CHAR);
  }

  while (!perma_eof && !th->err) {
    /* Wait until we're supposed to look for input: */
    WaitForSingleObject(th->checking_sema, INFINITE);

    WaitForSingleObject(th->lock_sema, INFINITE);
    if (th->checking < 0)
      break;

    if (th->avail) {
      /* Spurious wake-up? */
      ReleaseSemaphore(th->lock_sema, 1, NULL);
    } else {
      int ok;
      ReleaseSemaphore(th->lock_sema, 1, NULL);
      if (!is_console)
	ok = ReadFile(th->fd, th->buffer, toget, &got, NULL);
      else
        ok = read_console_via_wide(th->fd, w_buffer, CONSOLE_BUFFER_IN_SIZE, &w_buf_count,
                                   th->buffer, RKTIO_FD_BUFFSIZE, &got);
      if (ok) {
	WaitForSingleObject(th->lock_sema, INFINITE);
	th->avail = got;
	th->offset = 0;
	if (!got) {
	  /* We interpret a send of 0 bytes as a mid-stream EOF. */
	  eof_wait = CreateSemaphore(NULL, 0, 1, NULL);
	  th->eof = eof_wait;
	}
	/* lock is still held... */
      } else {
	int err;
	err = GetLastError();
	WaitForSingleObject(th->lock_sema, INFINITE);
	if (err == ERROR_BROKEN_PIPE) {
	  th->eof = INVALID_HANDLE_VALUE;
	  perma_eof = 1;
	} else
	  th->err = err;
	/* lock is still held... */
      }

      th->checking = 0;

      /* Notify main program that we found something: */
      ReleaseSemaphore(th->ready_sema, 1, NULL);

      if (!perma_eof && !th->err) {
	ReleaseSemaphore(th->lock_sema, 1, NULL);
      
	if (eof_wait) {
	  WaitForSingleObject(eof_wait, INFINITE);
	  CloseHandle(eof_wait);
	  eof_wait = NULL;
	}
      }
    }
  }
  /* lock is still held on `break` out of loop */

  /* We have to clean up if the main program has abandoned us: */
  if (th->you_clean_up) {
    WindowsFDICleanup(th, th->you_clean_up);
  } else {
    /* otherwise, main program is responsible for clean-up */
    th->you_clean_up = 1;
    ReleaseSemaphore(th->lock_sema, 1, NULL);
  }

  return 0;
}

static void WindowsFDICleanup(Win_FD_Input_Thread *th, int close_mode)
{
  CloseHandle(th->lock_sema);
  CloseHandle(th->checking_sema);
  CloseHandle(th->ready_sema);

  if (close_mode != -1)
    CloseHandle(th->fd);

  free(th->buffer);
  free(th);
}

/* result is 0 for fail, non-zero for success */
int read_console_via_wide(HANDLE fd,
                          /* a buffer for pending wide characters; limit the amount of
                             lookahead via `w_fill_len`; the `*_w_buf_count`argument
                             says how many chars are there on entry and exit */
                          wchar_t *w_buffer, DWORD w_fill_len, int *_w_buf_count,
                          /* a buffer to receive encoded UTF-8; this
                             buffer needs to be at least `MIN_VIA_WIDE_BUFFER_SIZE` bytes
                             to ensure decoding progress */
                          void *dest_buffer, DWORD dest_len, DWORD *_got) {
  int w_buf_count = *_w_buf_count;
  int ok;
  DWORD got;
  
  if (w_buf_count) {
    ok = 1;
    got = w_buf_count;
  } else
    ok = ReadConsoleW(fd, w_buffer, w_fill_len, &got, NULL);
  if (ok) {
    /* check for Ctl-Z, and convert it to an EOF */
    int i, move_start = 0, move_len = 0;
    for (i = 0; i < got; i++) {
      if (w_buffer[i] == 26 /* ctl-z */) {
        move_start = i;
        move_len = got - i;
        if (i == 0) {
          /* report EOF now */
          move_start++;
          move_len--;
        }
        got = i;
        break;
      }
    }
    got = WideCharToMultiByte(CP_UTF8, 0, w_buffer, got, dest_buffer, dest_len, NULL, 0);
    if (move_len > 0)
      memmove(w_buffer, w_buffer + move_start, sizeof(wchar_t) * move_len);
    w_buf_count = move_len;
  }

  *_w_buf_count = w_buf_count;
  *_got = got;

  return ok;
}

#endif

/*========================================================================*/
/* writing                                                                */
/*========================================================================*/

static intptr_t do_write(rktio_t *rktio, rktio_fd_t *rfd, const char *buffer, intptr_t len,
                         rktio_err_t *err)
{
#ifdef RKTIO_SYSTEM_UNIX
  int flags, errsaved;
  intptr_t amt;

# ifdef RKTIO_USE_IO_URING
  if (rfd->u)
    return uring_write(rktio, rfd, buffer, len, err);
# endif

  if (rfd->modes & RKTIO_OPEN_SOCKET)
    return rktio_socket_write(rktio, rfd, buffer, len, err);

# ifdef RKTIO_USE_PENDING_OPEN
  if (rfd->pending) {
    int errval = rktio_pending_open_poll(rktio, rfd, rfd->pending, err);
    if (errval) {
      errno = errval;
      rktio_get_posix_error(err);
      return RKTIO_WRITE_ERROR;
    } else if (rfd->pending)
      return 0;
  }
# endif

  flags = fcntl(rfd->fd, F_GETFL, 0);
  if (!(flags & RKTIO_NONBLOCKING))
    fcntl(rfd->fd, F_SETFL, flags | RKTIO_NONBLOCKING);

  amt = LIMIT_REQUEST_SIZE(len);

  do {
    do {
      len = write(rfd->fd, buffer, amt);
    } while ((len == -1) && (errno == EINTR));
    
    /* If there was no room to write `amt` bytes, then it's
       possible that writing fewer bytes will succeed. That seems
       to be the case with FIFOs on Mac OS X, for example. */
    amt = amt >> 1;
  } while ((len == -1) && (errno == EAGAIN) && (amt > 0));

  if (len == -1) {
    errsaved = errno;
    rktio_get_posix_error(err);
  } else
    errsaved = 0;

  if (!(flags & RKTIO_NONBLOCKING))
    fcntl(rfd->fd, F_SETFL, flags);

  if ((rfd->modes & RKTIO_OPEN_TRACK_TERMINAL_OUTPUT) && (len > 0))
    wrote_to_terminal(len);

  if (len == -1) {
    if (errsaved == EAGAIN)
      return 0;
    else
      return RKTIO_WRITE_ERROR;
  } else
    return len;
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  DWORD winwrote;

  if (rfd->modes & RKTIO_OPEN_SOCKET)
    return rktio_socket_write(rktio, rfd, buffer, len, err);

  force_console(rfd);

  if (rktio_fd_is_regular_file(rktio, rfd)
      || rktio_fd_is_terminal(rktio, rfd)) {
    /* Regular files never block, so this code looks like the Unix
       code. */

    /* If we try to write too much at once, the result
       is ERROR_NOT_ENOUGH_MEMORY (as opposed to a partial write). */
    int ok, to_console, can_leftover = 0, keep_leftover = 0;
    intptr_t towrite = len;
    const char *orig_buffer = buffer;
    wchar_t *w_buffer = NULL;
    DWORD max_winwrote;
    int errv;

    if (rfd->modes & RKTIO_OPEN_TEXT)
      buffer = adjust_output_text(buffer, &towrite);

    max_winwrote = towrite;
  
    to_console = rktio_fd_is_terminal(rktio, rfd);
    if (to_console) {
      /* Decode UTF-8 and write a chunk on a character boundary. */
      w_buffer = convert_output_wtext(buffer, &towrite,
				      &can_leftover, &keep_leftover,
				      rfd->leftover_len, rfd->leftover,
				      1024);
    }
    
    while (1) {
      if (!to_console)
	ok = WriteFile((HANDLE)rfd->fd, buffer, towrite, &winwrote, NULL);
      else {
	if (towrite)
	  ok = WriteConsoleW((HANDLE)rfd->fd, w_buffer, towrite, &winwrote, NULL);
	else {
	  /* can happen if can_leftover is > 0 */
	  ok = 1;
	  winwrote = 0;
	}
      }

      if (!ok)
        errv = GetLastError();

      if (!ok && (errv == ERROR_NOT_ENOUGH_MEMORY)) {
        towrite = towrite >> 1;
        if (towrite && (buffer != orig_buffer)) {
          /* don't write half of a CRLF: */
          if ((buffer[towrite-1] == '\r') && (buffer[towrite-1] == '\n'))
            --towrite;
        }
        if (!towrite)
          break;
      } else
        break;
    }

    if (!ok) {
      rktio_get_windows_error(err);
      if (buffer != orig_buffer)
	free((char *)buffer);
      return RKTIO_WRITE_ERROR;
    }

    if (to_console) {
      /* Convert wchar count to byte count, taking into account leftovers */
      int wrote_all = (winwrote == towrite);
      if (winwrote) {
	/* Recounting only works right if the output was well-formed
	   UTF-8. Weird things happen otherwise... but we guard against
	   external inconsistency with the `max_winwrote` check below. */
	winwrote = recount_output_wtext(w_buffer, winwrote);
	if (winwrote > rfd->leftover_len)
	  winwrote -= rfd->leftover_len;
	else
	  winwrote = 0;
	rfd->leftover_len = 0;
      }
      if (wrote_all && can_leftover) {
	memcpy(rfd->leftover + keep_leftover, buffer + max_winwrote - can_leftover, can_leftover);
	rfd->leftover_len = can_leftover + keep_leftover;
	winwrote += can_leftover;
      }
      free(w_buffer);
      if (winwrote > max_winwrote)
	winwrote = max_winwrote;
    }

    if (buffer != orig_buffer) {
      /* Convert converted count back to original count: */
      winwrote = recount_output_text(orig_buffer, buffer, winwrote);
      free((char *)buffer);
    }

    if ((rfd->modes & RKTIO_OPEN_TRACK_TERMINAL_OUTPUT) && (winwrote > 0))
      wrote_to_terminal(winwrote);

    return winwrote;
  } else {
    /* If we don't have a thread yet, we'll need to start it. If
       we have a non-blocking pipe, we can try the write (and
       we'll still need the thread to determine when the data is
       flushed). */
    intptr_t out_len = 0;
    int ok, errsaved;

    if (!rfd->oth || rfd->oth->nonblocking) {
      int nonblocking;

      /* If we don't have a thread, this is our first write attempt.
         Determine whether this is a non-blocking pipe: */
      if (!rfd->oth) {
        /* The FILE_TYPE_PIPE test is currently redundant, I think,
           but better safe than sorry. */
        nonblocking = (rktio->windows_nt_or_later
                       && (GetFileType((HANDLE)rfd->fd) == FILE_TYPE_PIPE));
      } else
        nonblocking = 1; /* must be, or we would not have gotten here */
    
      if (nonblocking) {
        /* Unless we're still trying to flush old data, write to the
           pipe and have the other thread start flushing it. */
        int flushed;

        if (rfd->oth) {
          if (rfd->oth->needflush) {
	    /* Not flushed, but we haven't promised to make progress: */
            flushed = 1;
          } else {
            WaitForSingleObject(rfd->oth->lock_sema, INFINITE);
            flushed = rfd->oth->flushed;
            ReleaseSemaphore(rfd->oth->lock_sema, 1, NULL);
          }
        } else
          flushed = 1; /* haven't written anything before */

        if (flushed) {
          /* Put the pipe in non-blocking mode and write. */
          int towrite;

          towrite = len;

          /* Apparently, the semantics of non-blocking pipe writes
             is not partial writes, but giving up entirely when
             the other end isn't being read. In other words, if we
             try to write too much and nothing is being pulled
             from the pipe, winwrote will be set to 0. Also, if
             we try to write too much at once, the result is a
             ERROR_NOT_ENOUGH_MEMORY error. Account for these
             behaviors by trying to write less each iteration when the
             write fails. (Yuck.) */
          while (1) {
            if (!rfd->unblocked) {
	      DWORD nonblock = PIPE_NOWAIT;
              ok = SetNamedPipeHandleState((HANDLE)rfd->fd, &nonblock, NULL, NULL);
              if (ok)
                rfd->unblocked = 1;
              else {
                errsaved = GetLastError();
		if (errsaved == ERROR_INVALID_FUNCTION) {
		  /* The handle (not a pipe?) doesn't support non-blocking mode. But
		     since we only try to write when the pipe is flushed, we can just
		     keep each request under the buffer size. */
		  DWORD bufsz;
		  if (GetNamedPipeInfo((HANDLE)rfd->fd, NULL, &bufsz, NULL, NULL)
		      && (bufsz > 0))
		    rfd->write_limit = bufsz;
		  else {
		    /* 256 should be small enough? */
		    rfd->write_limit = 256;
		  }
		  rfd->unblocked = 1;
		  ok = 1;
		} 
	      }
            } else
              ok = 1;

	    if (rfd->write_limit && (towrite > rfd->write_limit))
	      towrite = rfd->write_limit;

            if (ok) {
              ok = WriteFile((HANDLE)rfd->fd, buffer, towrite, &winwrote, NULL);
              if (!ok)
                errsaved = GetLastError();
            }

            if ((ok && !winwrote)
                || (!ok && (errsaved == ERROR_NOT_ENOUGH_MEMORY))
                /* it doesn't seem like `ERROR_PIPE_BUSY` should happen
                   in response to `WriteFile`, but it seems like this
                   might be happening (and this check is harmless if
                   it can't happen) */
                || (!ok && (errsaved == ERROR_PIPE_BUSY))) {
              towrite = towrite >> 1;
              if (!towrite) {
		/* leave winwrote as 0 */
                ok = 1;
		break;
              }
            } else
              break;
          }
        } else {
          /* Don't try to write while flushing. */
          ok = 1;
          winwrote = 0;
        }

	if (ok && winwrote && rfd->oth)
	  rfd->oth->needflush = 1;

        if (ok)
          out_len = winwrote;
      } else {
	/* Claim success for now to make `rfd->oth` get created,
	   and continuae non-blocking handling after that. */
	ok = 1;
	out_len = 1;
      }
      /* and create the writer thread... */

      if (ok && (out_len > 0) && !rfd->oth) {
        /* We create a thread even for pipes that can be put in
           non-blocking mode, because that seems to be the only
           way to get evt behavior of knowing that a write will
	   succeed because the previous content is flushed. */
        Win_FD_Output_Thread *oth;
        HANDLE h;
        DWORD id;
        unsigned char *bfr;
        HANDLE sm;

        oth = calloc(1, sizeof(Win_FD_Output_Thread));
        rfd->oth = oth;

        oth->nonblocking = nonblocking;

        if (!nonblocking) {
	  /* Create the buffer to communicate with the writing thread. */
          bfr = malloc(RKTIO_FD_BUFFSIZE);
          oth->buffer = bfr;
          oth->flushed = 0;
          oth->needflush = 0;
        } else {
	  /* No buffer needed */
          oth->buffer = NULL;
	  /* Some data was written, so it's not yet flushed,
	     and we need a flush. */
          oth->flushed = 0;
          oth->needflush = 1;
        }

        oth->buflen = 0;

        oth->fd = (HANDLE)rfd->fd;
        oth->err_no = 0;
        oth->done = 0;
        sm = CreateSemaphore(NULL, 1, 1, NULL);
        oth->lock_sema = sm;
        sm = CreateSemaphore(NULL, 0, 1, NULL);
        oth->work_sema = sm;
        sm = CreateSemaphore(NULL, 1, 1, NULL);
        oth->ready_sema = sm;

        h = CreateThread(NULL, 4096, (LPTHREAD_START_ROUTINE)WindowsFDWriter, oth, 0, &id);

        oth->thread = h;
      }
    }

    /* We have a thread, if only to watch when the flush is
       done... */
    
    if (rfd->oth && !rfd->oth->nonblocking) {
      /* This case is for Win 95/98/Me anonymous pipes and
         character devices.  We haven't written anything yet! We
         write to a buffer read by the other thread, and return --
         the other thread takes care of writing. Thus, as long as
         there's room in the buffer, we don't block, and we can
         tell whether there's room. Technical problem: if multiple
         ports are attched to the same underlying pipe (different
         handle, same "device"), the port writes can get out of
         order. We try to avoid the problem by sleeping. */

      /* At this point, `out_len` is set ot 1, and we
	 need to set it to a  value. */

      Win_FD_Output_Thread *oth = rfd->oth;

      WaitForSingleObject(oth->lock_sema, INFINITE);
      if (oth->err_no) {
        errsaved = oth->err_no;
        ok = 0;
      } else if (oth->buflen == RKTIO_FD_BUFFSIZE) {
	/* buffer is full, so can't write */
	out_len = 0;
        ok = 1;
      } else {
        winwrote = RKTIO_FD_BUFFSIZE - oth->buflen;
        if ((intptr_t)winwrote > len)
          winwrote = len;

        memcpy(oth->buffer + oth->buflen, buffer, winwrote);
        oth->buflen += winwrote;
        out_len = winwrote;

        /* Let the other thread know that it should start trying
           to write, if it isn't already: */
        ReleaseSemaphore(oth->work_sema, 1, NULL);
        Sleep(0); /* to decrease the chance of re-ordering flushes */

        ok = 1;
      }
      ReleaseSemaphore(oth->lock_sema, 1, NULL);
    }

    if (ok)
      return out_len;

    rktio_set_windows_error(err, errsaved);
    return RKTIO_WRITE_ERROR;
  }
#endif
}

intptr_t rktio_copy_fd(rktio_t *rktio, rktio_fd_t *src, rktio_fd_t *dest, intptr_t amt)
{
#if defined(RKTIO_SYSTEM_UNIX) && defined(__linux__)
  ssize_t r;
  int dest_regular = rktio_fd_is_regular_file(rktio, dest);

  if (!rktio_fd_is_regular_file(rktio, src)
      || (src->modes & RKTIO_OPEN_SOCKET)) {
    rktio_set_racket_error(&rktio->err, RKTIO_ERROR_UNSUPPORTED);
    return RKTIO_WRITE_ERROR;
  }

  if (!dest_regular && !(dest->modes & RKTIO_OPEN_SOCKET)) {
    /* `sendfile` would block on a blocking pipe, so require
       non-blocking mode */
    int flags = fcntl(dest->fd, F_GETFL, 0);
    if ((flags == -1) || !(flags & RKTIO_NONBLOCKING)) {
      rktio_set_racket_error(&rktio->err, RKTIO_ERROR_UNSUPPORTED);
      return RKTIO_WRITE_ERROR;
    }
  }

  amt = LIMIT_REQUEST_SIZE(amt);
  if (amt <= 0)
    return 0;

  do {
    r = sendfile(dest->fd, src->fd, NULL, amt);
  } while ((r == -1) && (errno == EINTR));

  if (r == -1) {
    if (errno == EAGAIN)
      return 0;
    else if ((errno == EINVAL) || (errno == ENOSYS) || (errno == EOPNOTSUPP)) {
      rktio_set_racket_error(&rktio->err, RKTIO_ERROR_UNSUPPORTED);
      return RKTIO_WRITE_ERROR;
    } else {
      rktio_get_posix_error(&rktio->err);
      return RKTIO_WRITE_ERROR;
    }
  } else if (r == 0)
    return RKTIO_READ_EOF;
  else
    return r;
#else
  rktio_set_racket_error(&rktio->err, RKTIO_ERROR_UNSUPPORTED);
  return RKTIO_WRITE_ERROR;
#endif
}

intptr_t rktio_write(rktio_t *rktio, rktio_fd_t *rfd, const char *buffer, intptr_t len)
{
  return do_write(rktio, rfd, buffer, len, &rktio->err);
}

intptr_t rktio_write_in(rktio_t *rktio, rktio_fd_t *rfd, const char *buffer, intptr_t start, intptr_t end)
{
  return rktio_write(rktio, rfd, buffer+start, end-start);
}

rktio_result_t *rktio_write_in_r(rktio_t *rktio, rktio_fd_t *rfd, const char *buffer, intptr_t start, intptr_t end)
{
  return maybe_success(do_write(rktio, rfd, buffer+start, end-start, &rfd->res.err), &rfd->res, RKTIO_WRITE_ERROR);
}

void rktio_std_write_in_best_effort(rktio_t *rktio, int which, char *buffer, intptr_t start, intptr_t end) {
#ifdef RKTIO_SYSTEM_UNIX
  intptr_t amt, len;

  while (start < end) {
    do {
      amt = end - start;
      amt = LIMIT_REQUEST_SIZE(amt);

      do {
        len = write(which, buffer + start, amt);
      } while ((len == -1) && (errno == EINTR));
    
      amt = amt >> 1;
    } while ((len == -1) && (errno == EAGAIN) && (amt > 0));

    if (len == -1)
      return;

    if (len > 0)
      wrote_to_terminal(len);

    start += len;
  }
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  HANDLE h;
  int ok, can_leftover = 0, keep_leftover = 0;
  intptr_t towrite = end - start, amt;
  wchar_t *w_buffer;
  DWORD winwrote;
  DWORD mode;
  int err;

  switch (which) {
  case RKTIO_STDOUT:
    which = STD_OUTPUT_HANDLE;
    break;
  case RKTIO_STDERR:
    which = STD_ERROR_HANDLE;
    break;
  }

  h = get_std_handle(which);  
  if ((h == INVALID_HANDLE_VALUE) || (h == NULL)) {
    rktio_create_console();
    h = get_std_handle(which);
    if ((h == INVALID_HANDLE_VALUE) || (h == NULL))
      return;
  }

  if ((GetFileType(h) == FILE_TYPE_CHAR) && GetConsoleMode(h, &mode)) {
    /* Decode UTF-8 */
    w_buffer = convert_output_wtext(buffer + start, &towrite,
                                    &can_leftover, &keep_leftover,
                                    0, NULL,
                                    0);
    
    start = 0;
    amt = towrite;
    while (towrite > 0) {
      ok = WriteConsoleW(h, w_buffer + start, amt, &winwrote, NULL);
      
      if (!ok) {
        err = GetLastError();

        if (err == ERROR_NOT_ENOUGH_MEMORY) {
          amt = amt >> 1;
          if (!amt) {
            towrite = 0;
            can_leftover = 0;
          }
        } else {
          towrite = 0;
          can_leftover = 0;
        }
      } else {
        start += winwrote;
        towrite -= winwrote;
        amt = towrite;
        if (winwrote > 0)
          wrote_to_terminal(winwrote);
      }
    }

    free(w_buffer);
  } else {
    can_leftover = towrite;
  }

  amt = can_leftover;
  while (can_leftover > 0) {
    ok = WriteFile(h, buffer + end - can_leftover, amt, &winwrote, NULL);
    
    if (!ok) {
      err = GetLastError();

      if ((err == ERROR_NOT_ENOUGH_MEMORY)
          || (err == ERROR_PIPE_BUSY)) {
        amt = amt >> 1;
        if (!amt) {
          can_leftover = 0;
        }
      } else {
        can_leftover = 0;
      }
    } if (!winwrote) {
      amt = amt >> 1;
      if (!amt) {
        can_leftover = 0;
      }
    } else {
      can_leftover -= winwrote;
      amt = can_leftover;
      wrote_to_terminal(winwrote);
    }
  }
#endif
}

#ifdef RKTIO_SYSTEM_WINDOWS

static const char *adjust_output_text(const char *buffer, intptr_t *towrite)
{
  intptr_t len = *towrite, i, j, newlines = 0;
  char *new_buffer;

  /* Limit work done here to avoid O(N^2) work if a client tried
     repeatedly to write a O(N)-sized buffer and only part goes out
     each time. */
  if (len > 4096)
    len = 4096;

  for (i = 0; i < len; i++) {
    if (buffer[i] == '\n')
      newlines++;
  }

  if (!newlines)
    return buffer;

  new_buffer = malloc(len + newlines);
  *towrite = len + newlines;

  for (i = 0, j = 0; i < len; i++) {
    if (buffer[i] == '\n') {
      new_buffer[j++] = '\r';
      new_buffer[j++] = '\n';
    } else
      new_buffer[j++] = buffer[i];
  }

  return new_buffer;
}

static intptr_t recount_output_text(const char *orig_buffer, const char *buffer, intptr_t wrote)
{
  intptr_t i = 0, j = 0;

  while (j < wrote) {
    if (orig_buffer[i] == '\n')
      j += 2;
    else
      j++;
    i++;
  }

  return i;
}

static wchar_t *convert_output_wtext(const char *buffer, intptr_t *_towrite,
				     int *_can_leftover, int *_keep_leftover,
				     int leftover_len, char *leftover,
				     int max_convert)
{
  /* Figure out how many bytes we can convert to complete wide
     characters. To avoid quadratic behavior overall, we'll limit the
     number of bytes.

     The given `leftover_len` and `leftover` is a prefix on `buffer`.
     If the tail (after writing all other bytes) is an incomplete
     UTF-8 prefix, report the prefix length in `_can_leftover`. */
  intptr_t i, count, len = *_towrite;
  char *src_buffer;
  wchar_t *dest_buffer;
  int want, span = 0;

  if (leftover_len) {
    /* Assume that leftover is a valid prefix: */
    int v = ((unsigned char *)leftover)[0];
    if ((v & 0xF8) == 0xF0)
      span = 4;
    else if ((v & 0xF0) == 0xE0)
      span = 3;
    else
      span = 2;
  } else
    span = 0;
  want = span - leftover_len;

  for (i = 0, count = 0; (i < len) && (!max_convert || (count < max_convert)); i++) {
    int v = ((unsigned char *)buffer)[i];
    if (want) {
      if ((v & 0xC0) == 0x80) {
	/* valid continuation byte */
	want--;
	if (!want) {
	  count++;
	  if (span == 4)
	    count++; /* surrogate pair */
	}
      } else {
	/* not a valid continuation byte */
	count++;
	want = 0;
	--i; /* retry byte */
      }
    } else if (!(v & 0x80)) {
      count++;
    } else if ((v & 0xF8) == 0xF0) {
      span = 4;
      want = 3;
    } else if ((v & 0xF0) == 0xE0) {
      span = 3;
      want = 2;
    } else if ((v & 0xE0) == 0xC0) {
      span = 2;
      want = 1;
    } else {
      /* bad continuation byte */
      count++;
    }
  }

  if ((i == len) && (want > 0)) {
    /* consuming all input, so set leftover */
    int keep = span - want;
    if (i >= keep) {
      *_can_leftover = keep;
      i -= keep;
      *_keep_leftover = 0;
    } else {
      *_can_leftover = (keep - leftover_len);
      *_keep_leftover = leftover_len;
      i = 0;
    }
  } else {
    *_can_leftover = 0;
    *_keep_leftover = 0;
  }

  if (leftover_len) {
    src_buffer = malloc(i + leftover_len);
    memcpy(src_buffer, leftover, leftover_len);
    memcpy(src_buffer + leftover_len, buffer, i);
    i += leftover_len;
  } else
    src_buffer = (char *)buffer;

  dest_buffer = (wchar_t *)malloc(sizeof(wchar_t) * count);

  if (count > 0) {
    count = MultiByteToWideChar(CP_UTF8, 0, src_buffer, i, dest_buffer, count);
    if (!count) {
      /* force progress */
      ((wchar_t *)src_buffer)[0] = 0xFFFD;
      count = 1;
    }
  }
  *_towrite = count;

  if (leftover_len)
    free(src_buffer);

  return dest_buffer;
}

static intptr_t recount_output_wtext(wchar_t *w_buffer, intptr_t winwrote)
{
  return WideCharToMultiByte(CP_UTF8, 0, w_buffer, winwrote, NULL, 0, NULL, 0);
}

static void deinit_write_fd(rktio_fd_t *rfd, int full_close)
{
  if (rfd->oth) {
    CSI_proc csi;

    WaitForSingleObject(rfd->oth->lock_sema, INFINITE);

    csi = get_csi();

    if (csi) {
      /* See also call above */
      csi(rfd->oth->thread);
    }
    CloseHandle(rfd->oth->thread);
    rfd->oth->done = 1;
    ReleaseSemaphore(rfd->oth->work_sema, 1, NULL);

    /* Try to leave clean-up to the other thread: */
    if (rfd->oth->you_clean_up) {
      /* Other thread is already done, so we're stuck with clean-up: */
      WindowsFDOCleanup(rfd->oth, (full_close ? 1 : -1));
    } else {
      /* otherwise, thread is responsible for clean-up */
      rfd->oth->you_clean_up = (full_close ? 1 : -1);
      ReleaseSemaphore(rfd->oth->lock_sema, 1, NULL);
    }
  }
}

static long WINAPI WindowsFDWriter(Win_FD_Output_Thread *oth)
{
  DWORD towrite, wrote;
  int ok, more_work = 0, err_no;

  if (oth->nonblocking) {
    /* Non-blocking mode (Win NT pipes). Just handle flush requests. */
    WaitForSingleObject(oth->lock_sema, INFINITE);
    while (!oth->done) {
      ReleaseSemaphore(oth->lock_sema, 1, NULL);
      WaitForSingleObject(oth->work_sema, INFINITE);

      FlushFileBuffers(oth->fd);
      
      WaitForSingleObject(oth->lock_sema, INFINITE);
      oth->flushed = 1;
      ReleaseSemaphore(oth->ready_sema, 1, NULL);
    }
    /* lock held on loop exit */
  } else {
    /* Blocking mode. We do the writing work.  This case is for
       Win 95/98/Me anonymous pipes and character devices (such 
       as LPT1). */
    while (!oth->err_no) {
      if (!more_work)
	WaitForSingleObject(oth->work_sema, INFINITE);

      WaitForSingleObject(oth->lock_sema, INFINITE);

      if (oth->done)
	break;

      towrite = oth->buflen;
      
      ReleaseSemaphore(oth->lock_sema, 1, NULL);

      ok = WriteFile(oth->fd, oth->buffer, towrite, &wrote, NULL);
      if (!ok)
	err_no = GetLastError();
      else
	err_no = 0;

      WaitForSingleObject(oth->lock_sema, INFINITE);
      if (!ok) {
	oth->err_no = err_no;
      } else {
	oth->buflen -= wrote;
	if (oth->buflen)
	  memmove(oth->buffer, oth->buffer + wrote, oth->buflen);
	more_work = oth->buflen > 0;
      }
      if ((oth->buflen < RKTIO_FD_BUFFSIZE) || oth->err_no)
	ReleaseSemaphore(oth->ready_sema, 1, NULL);
      ReleaseSemaphore(oth->lock_sema, 1, NULL);
    }
    /* lock is still held on `break` out of loop */
  }
  
  if (oth->you_clean_up) {
    WindowsFDOCleanup(oth, oth->you_clean_up);
  } else {
    /* otherwise, main thread is responsible for clean-up */
    oth->you_clean_up = 1;
    ReleaseSemaphore(oth->lock_sema, 1, NULL);
  }

  return 0;
}

static void WindowsFDOCleanup(Win_FD_Output_Thread *oth, int close_mode)
{
  CloseHandle(oth->lock_sema);
  CloseHandle(oth->work_sema);

  if (close_mode != -1)
    CloseHandle(oth->fd);

  if (oth->buffer)
    free(oth->buffer);
  free(oth);
}

#endif

#if defined(RKTIO_SYSTEM_WINDOWS)
static long terminal_writes;
#else
static uintptr_t terminal_writes;
#endif

#if defined(RKTIO_SYSTEM_UNIX) && defined(RKTIO_USE_PTHREADS)
static int terminal_write_initialzed = 0;
static pthread_mutex_t terminal_writes_lock;
#endif

void rktio_init_terminal_tracking(void) {
#if defined(RKTIO_SYSTEM_UNIX) && defined(RKTIO_USE_PTHREADS)
  if (!terminal_write_initialzed) {
    pthread_mutex_init(&terminal_writes_lock, NULL);
    terminal_write_initialzed = 1;
  }
#endif
}

static void wrote_to_terminal(intptr_t amt)
{
#if defined(RKTIO_SYSTEM_UNIX) && defined(RKTIO_USE_PTHREADS)
  pthread_mutex_lock(&terminal_writes_lock);
  terminal_writes += (uintptr_t)amt;
  pthread_mutex_unlock(&terminal_writes_lock);
#elif defined(RKTIO_SYSTEM_WINDOWS)
  InterlockedExchangeAdd(&terminal_writes, (long)amt);
#else
  terminal_writes += amt;
#endif
}

uintptr_t rktio_current_terminal_position(void)
{
  uintptr_t c;
#if defined(RKTIO_SYSTEM_UNIX) && defined(RKTIO_USE_PTHREADS)
  pthread_mutex_lock(&terminal_writes_lock);
  c = terminal_writes;
  pthread_mutex_unlock(&terminal_writes_lock);
#elif defined(RKTIO_SYSTEM_WINDOWS)
  c = (uintptr_t)InterlockedExchangeAdd(&terminal_writes, 0);
#else
  c = terminal_writes;
#endif
  return c;
}

/*========================================================================*/
/* console                                                                */
/*========================================================================*/

#ifdef RKTIO_SYSTEM_WINDOWS

#ifndef ENABLE_VIRTUAL_TERMINAL_PROCESSING
# define ENABLE_VIRTUAL_TERMINAL_PROCESSING  0x4
#endif

static HANDLE get_std_handle(int which) {
  HANDLE h;

  h = GetStdHandle(which);

  if ((h == INVALID_HANDLE_VALUE) || (h == NULL))
    return h;

  if ((which == STD_OUTPUT_HANDLE) || (which == STD_ERROR_HANDLE)) {
    if (GetFileType(h) == FILE_TYPE_CHAR) {
      /* Try to enable ANSI escape codes, which should work for a recent
         enough version of Windows */
      DWORD mode = 0;
      GetConsoleMode(h, &mode);
      SetConsoleMode(h, mode | ENABLE_VIRTUAL_TERMINAL_PROCESSING);
    }
  }

  return h;
}

static void force_console(rktio_fd_t *rfd) {
  /* DELAYED_CONSOLE_HANDLE is used to indicate that a console should be created on demand */
  if (rfd->fd == DELAYED_CONSOLE_HANDLE) {
    HANDLE h;
    int which;

    rktio_create_console();

    if (rfd->modes & RKTIO_OPEN_READ)
      which = STD_INPUT_HANDLE;
    else
      which = STD_OUTPUT_HANDLE;
    h = get_std_handle(which);

    rfd->fd = h;
  }
}

#endif

#!/usr/bin/env python3
# Add the yield and bias variants, the runtime flag override, and flag
# handling for every variant to hash-lock-variants' rumble/lock.ss
import sys
p = sys.argv[1]
s = open(p).read()

def rep(old, new, count=1):
    global s
    assert s.count(old) == count, (old, s.count(old))
    s = s.replace(old, new)

rep(""";;   seqflag  - `seq` with the single-thread flag
""", """;;   seqflag  - same as `seq`
;;   yield    - like `spin`, but waiters spin and then `sched_yield`
;;   bias     - each table is biased to the OS thread that creates it;
;;              the owner uses plain stores, and another thread revokes
;;              the bias once (with membarrier), after which the table
;;              uses the `yield` lock
;; Every variant except `master` skips locking until the place forks a
;; pthread; setting PLT_HASH_ALWAYS_LOCK at run time disables that.
""")

rep("""    (if (memq hash-lock-variant '(flag seqflag))
        #'(place-in-parallel?)
        #'#t))""", """    (if (eq? hash-lock-variant 'master)
        #'#t
        #'(place-in-parallel?)))""")

rep("""  ;; Waiting in the spinning variants: spin, then sleep with backoff
  (define-syntax-rule (spin-until test)
    (let loop ([n 0] [sleep-ns 1000])
      (cond
        [test (void)]
        [(fx< n 100) (loop (fx+ n 1) sleep-ns)]
        [else
         (#%sleep (#%make-time 'time-duration sleep-ns 0))
         (loop 0 (fxmin (fx* sleep-ns 2) 1000000))])))
""", """  (define sched-yield
    (let ([proc #f])
      (lambda ()
        (unless proc
          (#%load-shared-object "libc.so.6")
          (set! proc (foreign-procedure "sched_yield" () int)))
        (proc))))

  ;; Waiting in the spinning variants: spin, then sleep with backoff
  ;; (`spin`) or yield the CPU (the others)
  (define-syntax spin-until
    (lambda (stx)
      (syntax-case stx ()
        [(_ test)
         (if (eq? hash-lock-variant 'spin)
             #'(let loop ([n 0] [sleep-ns 1000])
                 (cond
                   [test (void)]
                   [(fx< n 100) (loop (fx+ n 1) sleep-ns)]
                   [else
                    (#%sleep (#%make-time 'time-duration sleep-ns 0))
                    (loop 0 (fxmin (fx* sleep-ns 2) 1000000))]))
             #'(let loop ([n 0])
                 (cond
                   [test (void)]
                   [(fx< n 100) (loop (fx+ n 1))]
                   [else
                    (sched-yield)
                    (loop 0)])))])))

  (meta-cond
   [(eq? hash-lock-variant 'bias)
    (define-record-type blk
      (fields state                ; box of #f or #t, for the revoked mode
              (mutable owner)      ; thread id, or #f after revocation
              (mutable inop)       ; #t while the owner is in an operation
              (mutable revoked))
      (nongenerative rumble-biased-lock-1))
    (define-syntax-rule (nonscheduler-lock? l) (blk? l))]
   [else
    (define-syntax-rule (nonscheduler-lock? l) (box? l))])
""")

rep("""    (box (meta-cond
          [(memq hash-lock-variant '(seq seqflag)) 0]
          [else #f])))""", """    (meta-cond
     [(eq? hash-lock-variant 'bias) (make-blk (box #f) (get-thread-id) #f #f)]
     [(memq hash-lock-variant '(seq seqflag)) (box 0)]
     [else (box #f)]))""")

rep("""          [(box? lock)
           ;; need to block""", """          [(nonscheduler-lock? lock)
           ;; need to block""")
rep("""          [(box? lock)
           (when (lock-needed?)
             (fast-release lock))""", """          [(nonscheduler-lock? lock)
           (when (lock-needed?)
             (fast-release lock))""")

rep("""   [(eq? hash-lock-variant 'spin)""", """   [(memq hash-lock-variant '(spin yield))""")

# readers in `spin`/`park` honor the flag, too
rep("""    (define-syntax-rule (with-eq-read-lock lock e)
      (let ([l lock])
        (cond
          [l
           (lock-acquire l)
           (let ([v e])
             (lock-release l)
             v)]
          [else e])))]""", """    (define-syntax-rule (with-eq-read-lock lock e)
      (let ([l lock])
        (cond
          [(and l (lock-needed?))
           (lock-acquire l)
           (let ([v e])
             (lock-release l)
             v)]
          [else e])))]""", 2)

rep("""   [(memq hash-lock-variant '(seq seqflag))
    ;; box of a fixnum""", """   [(eq? hash-lock-variant 'bias)
    ;; The owner sets `inop`, then checks `revoked`; a revoker sets
    ;; `revoked`, runs membarrier() so that every running thread of the
    ;; process executes a full fence, then waits for `inop` to clear.
    ;; So either the revoker sees the owner's `inop`, or the owner sees
    ;; `revoked`, without a fence on the owner's side. Only the owner
    ;; sets `inop`, so a release is in owner mode exactly when `inop` is
    ;; set and the caller is the owner.
    (define-syntax-rule (fast-acquire lock)
      (cond
        [(eqv? (blk-owner lock) (get-thread-id))
         (blk-inop-set! lock #t)
         (when (blk-revoked lock)
           (blk-inop-set! lock #f)
           (#%$app/no-inline acquire/shared lock))]
        [else (#%$app/no-inline acquire/nonowner lock)]))
    (define-syntax-rule (fast-release lock)
      (cond
        [(and (blk-inop lock) (eqv? (blk-owner lock) (get-thread-id)))
         (blk-inop-set! lock #f)]
        [else
         (memory-order-release)
         (set-box! (blk-state lock) #f)]))
    (define-box-lock-ops fast-acquire fast-release)
    (define (acquire/shared lock)
      (let ([b (blk-state lock)])
        (spin-until (and (not (unbox b))
                         (box-cas! b #f #t)))
        (memory-order-acquire)))
    (define (acquire/nonowner lock)
      (when (blk-owner lock)
        (blk-revoked-set! lock #t)
        (membarrier)
        (spin-until (not (blk-inop lock)))
        (blk-owner-set! lock #f))
      (acquire/shared lock))
    (define membarrier
      (let ([proc #f])
        (lambda ()
          (unless proc
            (#%load-shared-object "libc.so.6")
            (let ([sys (foreign-procedure (__varargs_after 1) "syscall" (long int unsigned-int int) long)])
              ;; 324 = SYS_membarrier on x86_64; 16 = MEMBARRIER_CMD_REGISTER_PRIVATE_EXPEDITED
              (unless (eqv? 0 (sys 324 16 0 0))
                (#%error 'membarrier "registration failed"))
              ;; 8 = MEMBARRIER_CMD_PRIVATE_EXPEDITED
              (set! proc (lambda ()
                           (unless (eqv? 0 (sys 324 8 0 0))
                             (#%error 'membarrier "failed"))))))
          (proc))))
    (define-syntax-rule (with-eq-read-lock lock e)
      (let ([l lock])
        (cond
          [(and l (lock-needed?))
           (lock-acquire l)
           (let ([v e])
             (lock-release l)
             v)]
          [else e])))]

   [(memq hash-lock-variant '(seq seqflag))
    ;; box of a fixnum""")

open(p, 'w').write(s)

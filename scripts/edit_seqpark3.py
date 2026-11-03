#!/usr/bin/env python3
# Add the seqpark3 and seqpark3b hash-lock variants to rumble/lock.ss
import sys
p = sys.argv[1]
s = open(p).read()

def rep(old, new, count=1):
    global s
    assert s.count(old) == count, (old, s.count(old))
    s = s.replace(old, new)

rep(""";; Every variant except `master` and the `seqpark` variants skips locking""",
""";;   seqpark3 - `seqpark2` with per-lock parking: writers park on the
;;              lock's own condition and a release wakes one of them,
;;              and readers spin, sleep briefly, then park until the
;;              writer finishes
;;   seqpark3b - `seqpark2b` with the same per-lock parking
;; Every variant except `master` and the `seqpark` variants skips locking""")

rep("""    (if (memq hash-lock-variant '(master seqpark seqpark2 seqpark2b))""",
    """    (if (memq hash-lock-variant '(master seqpark seqpark2 seqpark2b seqpark3 seqpark3b))""")

rep("""     [(memq hash-lock-variant '(seqpark2 seqpark2b)) (vector 0 0)]""",
    """     [(memq hash-lock-variant '(seqpark2 seqpark2b)) (vector 0 0)]
     [(memq hash-lock-variant '(seqpark3 seqpark3b)) (vector 0 0 #f)]""")

rep("""   [(memq hash-lock-variant '(seqpark2 seqpark2b))
    (define-syntax-rule (nonscheduler-lock? l) (#%vector? l))]""",
    """   [(memq hash-lock-variant '(seqpark2 seqpark2b seqpark3 seqpark3b))
    (define-syntax-rule (nonscheduler-lock? l) (#%vector? l))]""")

rep("""   [(memq hash-lock-variant '(seqpark2 seqpark2b))
    ;; A vector of two fixnums.""", """   [(memq hash-lock-variant '(seqpark3 seqpark3b))
    ;; A vector of three slots. Slot 0 is the writer lock: 0 when
    ;; unlocked, 1 when locked, and (for `seqpark3`) 3 when locked with
    ;; some thread possibly parked. Slot 1 is a seqlock counter that the
    ;; holder makes odd before changing the table and even again after.
    ;; Slot 2 is #f or the lock's parking state, created on first park.
    (define-record-type parking
      (fields mutex wcond rcond (mutable writers) (mutable readers))
      (nongenerative rumble-hash-lock-parking-1))
    (define (lock-parking lock)
      (or (#%vector-ref lock 2)
          (let ([pk (make-parking (make-mutex) (make-condition) (make-condition) 0 0)])
            (if (#%vector-cas! lock 2 #f pk)
                pk
                (#%vector-ref lock 2)))))
    ;; Counts for reporting through `vm-eval`; updates from different
    ;; locks can race, so they are approximate
    (define park-count 0)
    (define timeout-count 0)
    (define park-time (#%make-time 'time-duration 1000000 0))
    (define (timed-wait c m)
      (set! park-count (fx+ park-count 1))
      (unless (condition-wait c m park-time)
        (set! timeout-count (fx+ timeout-count 1))
        (#%set-top-level-value! 'hash-lock-park-timeouts timeout-count))
      (#%set-top-level-value! 'hash-lock-parks park-count))
    (define (wake-parked pk)
      (mutex-acquire (parking-mutex pk))
      (unless (eq? 0 (parking-writers pk))
        (condition-signal (parking-wcond pk)))
      (unless (eq? 0 (parking-readers pk))
        (condition-broadcast (parking-rcond pk)))
      (mutex-release (parking-mutex pk)))
    (define-syntax-rule (begin-write lock)
      (#%vector-set! lock 1 (fx+ (#%vector-ref lock 1) 1)))
    (define-syntax-rule (fast-acquire lock)
      (begin
        (unless (#%vector-cas! lock 0 0 1)
          (#%$app/no-inline lock-acquire/slow lock))
        (memory-order-acquire)
        (begin-write lock)))
    (define (lock-acquire/slow lock)
      (let loop ([n 0])
        (cond
          [(and (eq? 0 (#%vector-ref lock 0)) (#%vector-cas! lock 0 0 1)) (void)]
          [(fx< n 100) (loop (fx+ n 1))]
          [else (park-and-acquire lock)])))
    (meta-cond
     [(eq? hash-lock-variant 'seqpark3)
      ;; Release with a CAS that notices a parked thread
      (define-syntax-rule (fast-release lock)
        (begin
          (begin-write lock)
          (memory-order-release)
          (unless (#%vector-cas! lock 0 1 0)
            (#%$app/no-inline lock-release/slow lock))))
      (define (lock-release/slow lock)
        ;; slot 0 must be 3, and only the holder changes it from there;
        ;; whoever set 3 installed the parking state first
        (#%vector-set! lock 0 0)
        (wake-parked (#%vector-ref lock 2)))
      (define (park-and-acquire lock)
        (let ([pk (lock-parking lock)])
          (let loop ()
            (let ([v (#%vector-ref lock 0)])
              (cond
                [(eq? v 0)
                 ;; after parking, take the lock as 3, since other threads
                 ;; may still be parked
                 (unless (#%vector-cas! lock 0 0 3)
                   (loop))]
                [else
                 (when (or (eq? v 3) (#%vector-cas! lock 0 1 3))
                   (let ([m (parking-mutex pk)])
                     (mutex-acquire m)
                     (when (eq? 3 (#%vector-ref lock 0))
                       (parking-writers-set! pk (fx+ (parking-writers pk) 1))
                       (condition-wait (parking-wcond pk) m)
                       (parking-writers-set! pk (fx- (parking-writers pk) 1)))
                     (mutex-release m)))
                 (loop)])))))
      ;; A parking reader marks the lock so that the release wakes it
      (define-syntax-rule (reader-should-park? l)
        (and (fxodd? (#%vector-ref l 1))
             (let ([v (#%vector-ref l 0)])
               (or (eq? v 3)
                   (and (eq? v 1) (#%vector-cas! l 0 1 3))))))]
     [else
      ;; Release with a plain store, then check the parked counts.
      ;; Without a fence between the store and the loads, the releaser can
      ;; miss a thread that parks at the same moment, so parking is timed.
      (define-syntax-rule (fast-release lock)
        (begin
          (begin-write lock)
          (memory-order-release)
          (#%vector-set! lock 0 0)
          (let ([pk (#%vector-ref lock 2)])
            (when (and pk
                       (not (and (eq? 0 (parking-writers pk))
                                 (eq? 0 (parking-readers pk)))))
              (#%$app/no-inline wake-parked pk)))))
      (define (park-and-acquire lock)
        (let ([pk (lock-parking lock)])
          (let loop ()
            (cond
              [(and (eq? 0 (#%vector-ref lock 0)) (#%vector-cas! lock 0 0 1)) (void)]
              [else
               (let ([m (parking-mutex pk)])
                 (mutex-acquire m)
                 (parking-writers-set! pk (fx+ (parking-writers pk) 1))
                 (when (eq? 1 (#%vector-ref lock 0))
                   (timed-wait (parking-wcond pk) m))
                 (parking-writers-set! pk (fx- (parking-writers pk) 1))
                 (mutex-release m))
               (loop)]))))
      (define-syntax-rule (reader-should-park? l)
        (fxodd? (#%vector-ref l 1)))])
    (define-box-lock-ops lock-acquire lock-release fast-acquire fast-release)
    ;; A reader takes no lock; see `seq` about safety, which holds on
    ;; x86_64 only
    (define-syntax-rule (with-eq-read-lock lock e)
      (let ([l lock])
        (cond
          [l
           (let retry ()
             (let ([before (#%vector-ref l 1)])
               (cond
                 [(fxodd? before)
                  (#%$app/no-inline wait-for-writer l)
                  (retry)]
                 [else
                  (memory-order-acquire)
                  (let ([v e])
                    (memory-order-acquire)
                    (if (eq? before (#%vector-ref l 1))
                        v
                        (retry)))])))]
          [else e])))
    ;; Spin, sleep with backoff up to about 50 microseconds, then park
    (define (wait-for-writer l)
      (let loop ([n 0] [sleep-ns 1000])
        (cond
          [(fxeven? (#%vector-ref l 1)) (void)]
          [(fx< n 100) (loop (fx+ n 1) sleep-ns)]
          [(fx< sleep-ns 50000)
           (#%sleep (#%make-time 'time-duration sleep-ns 0))
           (loop 0 (fx* sleep-ns 2))]
          [else
           (park-reader l)
           (loop 0 sleep-ns)])))
    (define (park-reader l)
      (let* ([pk (lock-parking l)]
             [m (parking-mutex pk)])
        (mutex-acquire m)
        (parking-readers-set! pk (fx+ (parking-readers pk) 1))
        (when (reader-should-park? l)
          (timed-wait (parking-rcond pk) m))
        (parking-readers-set! pk (fx- (parking-readers pk) 1))
        (mutex-release m)))]

   [(memq hash-lock-variant '(seqpark2 seqpark2b))
    ;; A vector of two fixnums.""")

open(p, 'w').write(s)

#!/usr/bin/env python3
# Add the seqpark2 and seqpark2b hash-lock variants to rumble/lock.ss
import sys
p = sys.argv[1]
s = open(p).read()

def rep(old, new, count=1):
    global s
    assert s.count(old) == count, (old, s.count(old))
    s = s.replace(old, new)

rep(""";; Every variant except `master` and `seqpark` skips locking""", """;;   seqpark2 - `seqpark` with the lock and the counter in separate words,
;;              so a writer takes the lock with a blind CAS, and readers
;;              that wait sleep with backoff instead of locking
;;   seqpark2b - `seqpark2`, but a writer releases with a plain store and
;;              waiters park with a timed wait, since the releaser can
;;              miss a waiter that arrives at the same time
;; Every variant except `master` and the `seqpark` variants skips locking""")

rep("""    (if (memq hash-lock-variant '(master seqpark))
        #'#t""", """    (if (memq hash-lock-variant '(master seqpark seqpark2 seqpark2b))
        #'#t""")

rep("""     [(memq hash-lock-variant '(seq seqflag seqpark)) (box 0)]""",
    """     [(memq hash-lock-variant '(seq seqflag seqpark)) (box 0)]
     [(memq hash-lock-variant '(seqpark2 seqpark2b)) (vector 0 0)]""")

rep("""    (define-syntax-rule (nonscheduler-lock? l) (blk? l))]
   [else""", """    (define-syntax-rule (nonscheduler-lock? l) (blk? l))]
   [(memq hash-lock-variant '(seqpark2 seqpark2b))
    (define-syntax-rule (nonscheduler-lock? l) (#%vector? l))]
   [else""")

rep("""   [(eq? hash-lock-variant 'seqpark)
    ;; box of a fixnum""", """   [(memq hash-lock-variant '(seqpark2 seqpark2b))
    ;; A vector of two fixnums. Slot 0 is the writer lock: 0 when
    ;; unlocked, 1 when locked, and (for `seqpark2`) 3 when locked with
    ;; some writer possibly parked. Slot 1 is a seqlock counter that the
    ;; holder makes odd before changing the table and even again after.
    (define park-mutex (make-mutex))
    (define park-cond (make-condition))
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
     [(eq? hash-lock-variant 'seqpark2)
      ;; Release with a CAS that notices a parked writer
      (define-syntax-rule (fast-release lock)
        (begin
          (begin-write lock)
          (memory-order-release)
          (unless (#%vector-cas! lock 0 1 0)
            (#%$app/no-inline lock-release/slow lock))))
      (define (park-and-acquire lock)
        (let loop ()
          (let ([v (#%vector-ref lock 0)])
            (cond
              [(eq? v 0)
               ;; after parking, take the lock as 3, since other writers
               ;; may still be parked
               (unless (#%vector-cas! lock 0 0 3)
                 (loop))]
              [else
               (when (or (eq? v 3) (#%vector-cas! lock 0 1 3))
                 (mutex-acquire park-mutex)
                 (when (eq? 3 (#%vector-ref lock 0))
                   (condition-wait park-cond park-mutex))
                 (mutex-release park-mutex))
               (loop)]))))
      (define (lock-release/slow lock)
        ;; slot 0 must be 3, and only the holder changes it from there
        (#%vector-set! lock 0 0)
        (mutex-acquire park-mutex)
        (condition-broadcast park-cond)
        (mutex-release park-mutex))]
     [else
      ;; Release with a plain store, then check a count of parked writers.
      ;; Without a fence between the store and the load, the releaser can
      ;; miss a writer that parks at the same moment, so parking is timed.
      (define parked-count 0)
      (define timeout-count 0)
      (define park-count 0)
      (define park-time (#%make-time 'time-duration 1000000 0))
      (define-syntax-rule (fast-release lock)
        (begin
          (begin-write lock)
          (memory-order-release)
          (#%vector-set! lock 0 0)
          (unless (eq? 0 parked-count)
            (#%$app/no-inline wake-parked))))
      (define (wake-parked)
        (mutex-acquire park-mutex)
        (condition-broadcast park-cond)
        (mutex-release park-mutex))
      (define (park-and-acquire lock)
        (let loop ()
          (cond
            [(and (eq? 0 (#%vector-ref lock 0)) (#%vector-cas! lock 0 0 1)) (void)]
            [else
             (mutex-acquire park-mutex)
             (set! parked-count (fx+ parked-count 1))
             (when (eq? 1 (#%vector-ref lock 0))
               (set! park-count (fx+ park-count 1))
               (unless (condition-wait park-cond park-mutex park-time)
                 (set! timeout-count (fx+ timeout-count 1))
                 ;; visible from `vm-eval` for reporting
                 (#%set-top-level-value! 'hash-lock-park-timeouts timeout-count))
               (#%set-top-level-value! 'hash-lock-parks park-count))
             (set! parked-count (fx- parked-count 1))
             (mutex-release park-mutex)
             (loop)])))])
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
    (define (wait-for-writer l)
      (let loop ([n 0] [sleep-ns 1000])
        (cond
          [(fxeven? (#%vector-ref l 1)) (void)]
          [(fx< n 100) (loop (fx+ n 1) sleep-ns)]
          [else
           (#%sleep (#%make-time 'time-duration sleep-ns 0))
           (loop 0 (fxmin (fx* sleep-ns 2) 1000000))])))]

   [(eq? hash-lock-variant 'seqpark)
    ;; box of a fixnum""")

open(p, 'w').write(s)

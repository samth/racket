#!/usr/bin/env python3
# Add the seqpark hash-lock variant to rumble/lock.ss
import sys
p = sys.argv[1]
s = open(p).read()

def rep(old, new, count=1):
    global s
    assert s.count(old) == count, (old, s.count(old))
    s = s.replace(old, new)

rep(""";; Every variant except `master` skips locking""", """;;   seqpark  - `seq` readers, with writers that spin and then park, and
;;              no single-thread flag
;; Every variant except `master` and `seqpark` skips locking""")

rep("""    (if (eq? hash-lock-variant 'master)
        #'#t""", """    (if (memq hash-lock-variant '(master seqpark))
        #'#t""")

rep("""     [(memq hash-lock-variant '(seq seqflag)) (box 0)]""",
    """     [(memq hash-lock-variant '(seq seqflag seqpark)) (box 0)]""")

rep("""   [(memq hash-lock-variant '(seq seqflag))
    ;; box of a fixnum""", """   [(eq? hash-lock-variant 'seqpark)
    ;; box of a fixnum 4*gen + 2*waiters + locked: bit 0 is set while a
    ;; writer holds the lock, bit 1 when some writer may be parked, and
    ;; each release bumps `gen` and clears both bits. Only the holder
    ;; changes the value from a locked one, except that a waiter may set
    ;; bit 1. All parked writers share one mutex and condition.
    (define park-mutex (make-mutex))
    (define park-cond (make-condition))
    (define-syntax-rule (fast-acquire lock)
      (let ([v (unbox lock)])
        (cond
          [(and (fxeven? v) (box-cas! lock v (fx+ v 1))) (memory-order-acquire)]
          [else (#%$app/no-inline lock-acquire/slow lock)])))
    (define-syntax-rule (fast-release lock)
      (let* ([v (unbox lock)]
             [next (fx+ (fxand v -4) 4)])
        (memory-order-release)
        (unless (and (fx= 0 (fxand v 2))
                     (box-cas! lock v next))
          (#%$app/no-inline lock-release/slow lock next))))
    (define-box-lock-ops lock-acquire lock-release fast-acquire fast-release)
    (define (lock-acquire/slow lock)
      (let loop ([n 0])
        (let ([v (unbox lock)])
          (cond
            [(and (fxeven? v) (box-cas! lock v (fx+ v 1)))
             (memory-order-acquire)]
            [(fx< n 100) (loop (fx+ n 1))]
            [else (park-and-acquire lock)]))))
    (define (park-and-acquire lock)
      (let loop ()
        (let ([v (unbox lock)])
          (cond
            [(fxeven? v)
             ;; after parking, take the lock with bit 1 set, since other
             ;; writers may still be parked
             (if (box-cas! lock v (fx+ v 3))
                 (memory-order-acquire)
                 (loop))]
            [else
             (when (or (fx= 2 (fxand v 2))
                       (box-cas! lock v (fxior v 2)))
               (mutex-acquire park-mutex)
               (when (fx= 3 (fxand (unbox lock) 3))
                 (condition-wait park-cond park-mutex))
               (mutex-release park-mutex))
             (loop)]))))
    (define (lock-release/slow lock next)
      ;; bit 1 is set, and only the holder changes it from here
      (set-box! lock next)
      (mutex-acquire park-mutex)
      (condition-broadcast park-cond)
      (mutex-release park-mutex))
    ;; A reader takes no lock and ignores changes to bit 1; see `seq`
    ;; about safety, which holds on x86_64 only
    (define-syntax-rule (with-eq-read-lock lock e)
      (let ([l lock])
        (cond
          [l
           (let retry ()
             (let ([before (unbox l)])
               (cond
                 [(fxodd? before)
                  (#%$app/no-inline wait-for-writer l)
                  (retry)]
                 [else
                  (memory-order-acquire)
                  (let ([v e])
                    (memory-order-acquire)
                    (if (fx= (fxand before -3) (fxand (unbox l) -3))
                        v
                        (retry)))])))]
          [else e])))
    (define (wait-for-writer l)
      (let loop ([n 0])
        (cond
          [(fxeven? (unbox l)) (void)]
          [(fx< n 100) (loop (fx+ n 1))]
          [else
           ;; parks if the writer takes a while
           (lock-acquire l)
           (lock-release l)])))]

   [(memq hash-lock-variant '(seq seqflag))
    ;; box of a fixnum""")

open(p, 'w').write(s)

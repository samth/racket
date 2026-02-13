#lang racket/base

;; racket/cron - Cron expression parsing and in-process job scheduling.
;;
;; Provides:
;;   - Cron expression parsing (5-field standard format + nicknames)
;;   - Next-occurrence calculation
;;   - In-process cron job scheduler using background threads
;;
;; Modeled after Bun.cron / Deno.cron API design.

(require racket/contract/base
         racket/string)

;; ============================================================================
;; Scheduled event struct
;; ============================================================================

(struct scheduled-event (type cron scheduled-time) #:transparent)

;; ============================================================================
;; Cron expression struct (bitset representation)
;; ============================================================================

;; minutes:           integer, bits 0-59
;; hours:             integer, bits 0-23
;; days:              integer, bits 1-31
;; months:            integer, bits 1-12
;; weekdays:          integer, bits 0-6 (0=Sunday)
;; days-is-wildcard:  boolean
;; weekdays-is-wildcard: boolean
(struct cron-expr (minutes hours days months weekdays
                   days-is-wildcard? weekdays-is-wildcard?)
  #:transparent)

;; ============================================================================
;; Name lookup tables
;; ============================================================================

(define weekday-names
  (hash "sun" 0 "mon" 1 "tue" 2 "wed" 3 "thu" 4 "fri" 5 "sat" 6
        "sunday" 0 "monday" 1 "tuesday" 2 "wednesday" 3
        "thursday" 4 "friday" 5 "saturday" 6))

(define month-names
  (hash "jan" 1 "feb" 2 "mar" 3 "apr" 4 "may" 5 "jun" 6
        "jul" 7 "aug" 8 "sep" 9 "oct" 10 "nov" 11 "dec" 12
        "january" 1 "february" 2 "march" 3 "april" 4
        "june" 6 "july" 7 "august" 8 "september" 9
        "october" 10 "november" 11 "december" 12))

;; ============================================================================
;; Bit operations
;; ============================================================================

(define (bit-set? n pos)
  (bitwise-bit-set? n pos))

(define (bits-range min max)
  ;; Set bits from min to max (inclusive)
  (for/fold ([bits 0]) ([i (in-range min (add1 max))])
    (bitwise-ior bits (arithmetic-shift 1 i))))

(define all-minutes (bits-range 0 59))
(define all-hours (bits-range 0 23))
(define all-days (bits-range 1 31))
(define all-months (bits-range 1 12))
(define all-weekdays (bits-range 0 6))

;; ============================================================================
;; Nickname parsing
;; ============================================================================

(define (parse-nickname str)
  (define s (string-downcase str))
  (cond
    [(or (string=? s "@yearly") (string=? s "@annually"))
     (cron-expr (arithmetic-shift 1 0)    ; minute 0
                (arithmetic-shift 1 0)    ; hour 0
                (arithmetic-shift 1 1)    ; day 1
                (arithmetic-shift 1 1)    ; month 1
                all-weekdays #f #t)]
    [(string=? s "@monthly")
     (cron-expr (arithmetic-shift 1 0)
                (arithmetic-shift 1 0)
                (arithmetic-shift 1 1)
                all-months
                all-weekdays #f #t)]
    [(string=? s "@weekly")
     (cron-expr (arithmetic-shift 1 0)
                (arithmetic-shift 1 0)
                all-days
                all-months
                (arithmetic-shift 1 0)    ; Sunday
                #t #f)]
    [(or (string=? s "@daily") (string=? s "@midnight"))
     (cron-expr (arithmetic-shift 1 0)
                (arithmetic-shift 1 0)
                all-days all-months all-weekdays #t #t)]
    [(string=? s "@hourly")
     (cron-expr (arithmetic-shift 1 0)
                all-hours all-days all-months all-weekdays #t #t)]
    [else #f]))

;; ============================================================================
;; Field parsing
;; ============================================================================

;; Parse a single value (number or name), return integer or #f
(define (parse-value str min-val max-val kind)
  (cond
    ;; Try named lookup first
    [(eq? kind 'weekday)
     (define v (hash-ref weekday-names (string-downcase str) #f))
     (or v (parse-numeric-value str min-val max-val kind))]
    [(eq? kind 'month)
     (define v (hash-ref month-names (string-downcase str) #f))
     (or v (parse-numeric-value str min-val max-val kind))]
    [else (parse-numeric-value str min-val max-val kind)]))

(define (parse-numeric-value str min-val max-val kind)
  (define n (string->number str))
  (cond
    [(not n) #f]
    [(not (exact-nonnegative-integer? n)) #f]
    ;; Weekday 7 = Sunday (0)
    [(and (eq? kind 'weekday) (= n 7)) 0]
    [(or (< n min-val) (> n max-val)) #f]
    [else n]))

;; Find index of a character in a string, or #f
(define (string-index-of str ch)
  (for/first ([i (in-range (string-length str))]
              #:when (char=? (string-ref str i) ch))
    i))

;; Split a field into range parts: "1-5" -> '("1" "5"), "3" -> #f
(define (split-range str)
  (define idx (string-index-of str #\-))
  (cond
    [(not idx) #f]
    [(= idx 0) #f]
    [(= idx (sub1 (string-length str))) #f]
    [else
     (define rest (substring str (add1 idx)))
     ;; Ensure no second dash in rest
     (if (string-index-of rest #\-)
         #f
         (list (substring str 0 idx) rest))]))

;; Parse a single cron field (e.g. "1,5-10,*/3") into a bitset
(define (parse-field field min-val max-val kind)
  (when (string=? field "")
    (error 'cron-parse "empty field"))
  (define parts (string-split field "," #:trim? #f))
  (for/fold ([result 0]) ([part (in-list parts)])
    (when (string=? part "")
      (error 'cron-parse "empty element in list"))
    ;; Split by / for step
    (define slash-parts (string-split part "/" #:trim? #f))
    (when (> (length slash-parts) 2)
      (error 'cron-parse "invalid step expression: ~a" part))
    (define base (car slash-parts))
    (define step-str (and (= (length slash-parts) 2) (cadr slash-parts)))
    (define step
      (if step-str
          (let ([n (string->number step-str)])
            (when (or (not n) (not (exact-positive-integer? n)))
              (error 'cron-parse "invalid step value: ~a" step-str))
            n)
          1))

    (define-values (range-min range-max)
      (cond
        [(string=? base "*")
         (values min-val max-val)]
        [else
         (define range-parts (split-range base))
         (cond
           [range-parts
            (define lo (parse-value (car range-parts) min-val max-val kind))
            (define hi (parse-value (cadr range-parts) min-val max-val kind))
            (when (or (not lo) (not hi))
              (error 'cron-parse "invalid range value in: ~a" base))
            (when (> lo hi)
              (error 'cron-parse "invalid range ~a-~a" lo hi))
            (values lo hi)]
           [else
            (define v (parse-value base min-val max-val kind))
            (when (not v)
              (error 'cron-parse "invalid value: ~a" base))
            (values v (if step-str max-val v))])]))

    ;; Set bits with step
    (bitwise-ior
     result
     (for/fold ([bits 0]) ([i (in-range range-min (add1 range-max) step)])
       (bitwise-ior bits (arithmetic-shift 1 i))))))

;; ============================================================================
;; Main parser
;; ============================================================================

(define (cron-parse expr-str)
  (define expr (string-trim expr-str))

  ;; Check for nicknames
  (cond
    [(and (> (string-length expr) 0) (char=? (string-ref expr 0) #\@))
     (define result (parse-nickname expr))
     (unless result
       (error 'cron-parse "unknown nickname: ~a" expr))
     result]
    [else
     (define fields (string-split expr))
     (when (> (length fields) 5)
       (error 'cron-parse "too many fields (expected 5, got ~a)" (length fields)))
     (when (< (length fields) 5)
       (error 'cron-parse "too few fields (expected 5, got ~a)" (length fields)))

     (cron-expr
      (parse-field (list-ref fields 0) 0 59 'none)
      (parse-field (list-ref fields 1) 0 23 'none)
      (parse-field (list-ref fields 2) 1 31 'none)
      (parse-field (list-ref fields 3) 1 12 'month)
      (parse-field (list-ref fields 4) 0 6 'weekday)
      (string=? (list-ref fields 2) "*")
      (string=? (list-ref fields 4) "*"))]))

;; ============================================================================
;; Date/time helpers (UTC)
;; ============================================================================

;; Days in a given month (1-12) for a given year
(define (days-in-month month year)
  (case month
    [(1 3 5 7 8 10 12) 31]
    [(4 6 9 11) 30]
    [(2) (if (leap-year? year) 29 28)]
    [else 30]))

(define (leap-year? year)
  (or (and (zero? (modulo year 4))
           (not (zero? (modulo year 100))))
      (zero? (modulo year 400))))

;; Day of week for a date (0=Sunday). Uses Tomohiko Sakamoto's algorithm.
(define (day-of-week year month day)
  (define t (vector 0 3 2 5 0 3 5 1 4 6 2 4))
  (define y (if (< month 3) (sub1 year) year))
  (modulo (+ y
             (quotient y 4)
             (- (quotient y 100))
             (quotient y 400)
             (vector-ref t (sub1 month))
             day)
          7))

;; Convert seconds since epoch (UTC) to date components
;; Returns (values year month day hour minute second weekday)
(define (seconds->utc-components secs)
  (define d (seconds->date secs #f)) ; #f = UTC
  (values (date-year d) (date-month d) (date-day d)
          (date-hour d) (date-minute d) (date-second d)
          (date-week-day d)))

;; Convert UTC date components to seconds since epoch
(define (utc-components->seconds year month day hour minute second)
  (find-seconds-utc second minute hour day month year))

;; find-seconds in UTC (no DST issues)
(define (find-seconds-utc second minute hour day month year)
  ;; Normalize overflowed date components by using Racket's date functions
  ;; We use a simpler approach: compute Julian day number then convert
  (define jdn (gregorian->jdn year month day))
  (define epoch-jdn (gregorian->jdn 1970 1 1))
  (define day-diff (- jdn epoch-jdn))
  (+ (* day-diff 86400)
     (* hour 3600)
     (* minute 60)
     second))

;; Convert Gregorian date to Julian Day Number
(define (gregorian->jdn year month day)
  (define a (quotient (- 14 month) 12))
  (define y (+ year 4800 (- a)))
  (define m (+ month (* 12 a) -3))
  (+ day
     (quotient (+ (* 153 m) 2) 5)
     (* 365 y)
     (quotient y 4)
     (- (quotient y 100))
     (quotient y 400)
     -32045))

;; Normalize a date: handles overflows (e.g., day 32 -> next month)
(define (normalize-date year month day hour minute second)
  ;; Handle minute overflow
  (define-values (extra-hours norm-minute)
    (values (quotient minute 60) (modulo minute 60)))
  (define hour2 (+ hour extra-hours))
  ;; Handle hour overflow
  (define-values (extra-days norm-hour)
    (values (quotient hour2 24) (modulo hour2 24)))
  (define day2 (+ day extra-days))
  ;; Handle month overflow
  (define-values (extra-years norm-month0)
    (values (quotient (sub1 month) 12) (add1 (modulo (sub1 month) 12))))
  (define year2 (+ year extra-years))
  ;; Handle day overflow
  (let loop ([y year2] [m norm-month0] [d day2])
    (define dim (days-in-month m y))
    (cond
      [(> d dim)
       (define new-m (add1 m))
       (if (> new-m 12)
           (loop (add1 y) 1 (- d dim))
           (loop y new-m (- d dim)))]
      [(<= d 0)
       ;; Shouldn't happen in our use case, but handle it
       (define prev-m (sub1 m))
       (define-values (py pm)
         (if (<= prev-m 0)
             (values (sub1 y) 12)
             (values y prev-m)))
       (loop py pm (+ d (days-in-month pm py)))]
      [else (values y m d norm-hour norm-minute second)])))

;; ============================================================================
;; Next occurrence calculator
;; ============================================================================

;; Compute the next UTC time (as seconds since epoch) that matches
;; the cron expression, starting from from-secs.
;; Returns #f if no match found within ~4 years.
(define (cron-next expr from-secs)
  (define-values (yr mo dy hr mn sc _wd)
    (seconds->utc-components (exact-floor from-secs)))

  ;; Advance by 1 minute, zero out seconds
  (define mn2 (add1 mn))
  (define-values (start-yr start-mo start-dy start-hr start-mn start-sc)
    (normalize-date yr mo dy hr mn2 0))

  ;; Compute the weekday for the current date
  (define max-iterations (* 1500 24 60)) ; ~4 years of minutes

  (let loop ([year start-yr] [month start-mo] [day start-dy]
             [hour start-hr] [minute start-mn]
             [iter 0])
    (cond
      [(>= iter max-iterations) #f]
      [else
       ;; Normalize to handle overflows and compute weekday
       (define-values (ny nm nd nh nmin _ns)
         (normalize-date year month day hour minute 0))
       (define wd (day-of-week ny nm nd))

       (cond
         ;; Check month
         [(not (bit-set? (cron-expr-months expr) nm))
          (loop ny (add1 nm) 1 0 0 (add1 iter))]

         ;; Check day (POSIX OR logic)
         [(let ()
            (define day-ok (bit-set? (cron-expr-days expr) nd))
            (define wd-ok (bit-set? (cron-expr-weekdays expr) wd))
            (define both-restricted
              (and (not (cron-expr-days-is-wildcard? expr))
                   (not (cron-expr-weekdays-is-wildcard? expr))))
            (define day-match
              (if both-restricted
                  (or day-ok wd-ok)
                  (and day-ok wd-ok)))
            (not day-match))
          (loop ny nm (add1 nd) 0 0 (add1 iter))]

         ;; Check hour
         [(not (bit-set? (cron-expr-hours expr) nh))
          (loop ny nm nd (add1 nh) 0 (add1 iter))]

         ;; Check minute
         [(not (bit-set? (cron-expr-minutes expr) nmin))
          (loop ny nm nd nh (add1 nmin) (add1 iter))]

         ;; All fields match
         [else
          (utc-components->seconds ny nm nd nh nmin 0)])])))

;; ============================================================================
;; Format cron expression as normalized numeric string
;; ============================================================================

(define (format-bitfield bits min-val max-val)
  (define all-set?
    (for/and ([i (in-range min-val (add1 max-val))])
      (bit-set? bits i)))
  (cond
    [all-set? "*"]
    [else
     (string-join
      (for/list ([i (in-range min-val (add1 max-val))]
                 #:when (bit-set? bits i))
        (number->string i))
      ",")]))

(define (cron-expr->string expr)
  (string-join
   (list (format-bitfield (cron-expr-minutes expr) 0 59)
         (format-bitfield (cron-expr-hours expr) 0 23)
         (format-bitfield (cron-expr-days expr) 1 31)
         (format-bitfield (cron-expr-months expr) 1 12)
         (format-bitfield (cron-expr-weekdays expr) 0 6))
   " "))

;; ============================================================================
;; Cron job scheduler
;; ============================================================================

;; Internal state: hash of name -> job-info
(struct cron-job (name expr-str expr handler thread) #:mutable #:transparent)

(define cron-jobs (make-hash))
(define cron-lock (make-semaphore 1))

;; The scheduler thread for a single job
(define (make-cron-thread name expr expr-str handler)
  (thread
   (lambda ()
     (let loop ()
       (define now (current-seconds))
       (define next-time (cron-next expr now))
       (cond
         [(not next-time)
          ;; No next occurrence within ~4 years; stop
          (void)]
         [else
          (define delay-secs (max 0 (- next-time now)))
          (define delay-ms (* delay-secs 1000))
          ;; Sleep until next occurrence (using sync with alarm-evt for precision)
          (define target-ms (+ (current-inexact-milliseconds) delay-ms))
          (sync (alarm-evt target-ms))
          ;; Create scheduled event and invoke handler
          (define evt (scheduled-event "scheduled" expr-str
                                       (* next-time 1000))) ; ms since epoch
          (with-handlers ([exn:fail? (lambda (e)
                                       (log-error "cron job ~a failed: ~a"
                                                  name (exn-message e)))])
            (handler evt))
          (loop)])))))

;; Register a cron job. If a job with the same name exists, it is replaced.
(define (cron name schedule handler)
  (define expr (cron-parse schedule))
  (call-with-semaphore cron-lock
    (lambda ()
      ;; Remove existing job with same name if any
      (when (hash-has-key? cron-jobs name)
        (define old (hash-ref cron-jobs name))
        (define old-thread (cron-job-thread old))
        (when (and old-thread (thread-running? old-thread))
          (kill-thread old-thread)))
      ;; Create and start new job
      (define job-thread (make-cron-thread name expr schedule handler))
      (define job (cron-job name schedule expr handler job-thread))
      (hash-set! cron-jobs name job)
      (void))))

;; Remove a cron job by name
(define (cron-remove name)
  (call-with-semaphore cron-lock
    (lambda ()
      (when (hash-has-key? cron-jobs name)
        (define job (hash-ref cron-jobs name))
        (define t (cron-job-thread job))
        (when (and t (thread-running? t))
          (kill-thread t))
        (hash-remove! cron-jobs name))
      (void))))

;; List all registered cron job names
(define (cron-jobs-list)
  (call-with-semaphore cron-lock
    (lambda ()
      (hash-keys cron-jobs))))

;; Stop all cron jobs
(define (cron-stop-all)
  (call-with-semaphore cron-lock
    (lambda ()
      (for ([(name job) (in-hash cron-jobs)])
        (define t (cron-job-thread job))
        (when (and t (thread-running? t))
          (kill-thread t)))
      (hash-clear! cron-jobs)
      (void))))

;; ============================================================================
;; Provides
;; ============================================================================

(provide
 ;; Scheduled event
 (struct-out scheduled-event)

 ;; Cron expression
 (struct-out cron-expr)

 ;; Parsing and next-occurrence
 (contract-out
  [cron-parse (-> string? cron-expr?)]
  [cron-next (-> cron-expr? real? (or/c exact-integer? #f))]
  [cron-expr->string (-> cron-expr? string?)])

 ;; Job scheduler
 (contract-out
  [cron (-> string? string? (-> scheduled-event? any) void?)]
  [cron-remove (-> string? void?)]
  [cron-jobs-list (-> (listof string?))]
  [cron-stop-all (-> void?)]))

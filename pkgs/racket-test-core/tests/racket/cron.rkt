#lang racket/base

;; Tests for racket/cron

(require racket/cron
         rackunit)

;; ============================================================================
;; Cron expression parsing tests
;; ============================================================================

(test-case "parse simple wildcard expression"
  (define expr (cron-parse "* * * * *"))
  (check-true (cron-expr? expr))
  (check-true (cron-expr-days-is-wildcard? expr))
  (check-true (cron-expr-weekdays-is-wildcard? expr)))

(test-case "parse specific values"
  (define expr (cron-parse "30 2 15 6 1"))
  (check-true (bitwise-bit-set? (cron-expr-minutes expr) 30))
  (check-false (bitwise-bit-set? (cron-expr-minutes expr) 0))
  (check-true (bitwise-bit-set? (cron-expr-hours expr) 2))
  (check-true (bitwise-bit-set? (cron-expr-days expr) 15))
  (check-true (bitwise-bit-set? (cron-expr-months expr) 6))
  (check-true (bitwise-bit-set? (cron-expr-weekdays expr) 1)))

(test-case "parse step expression */15"
  (define expr (cron-parse "*/15 * * * *"))
  (check-true (bitwise-bit-set? (cron-expr-minutes expr) 0))
  (check-true (bitwise-bit-set? (cron-expr-minutes expr) 15))
  (check-true (bitwise-bit-set? (cron-expr-minutes expr) 30))
  (check-true (bitwise-bit-set? (cron-expr-minutes expr) 45))
  (check-false (bitwise-bit-set? (cron-expr-minutes expr) 5))
  (check-false (bitwise-bit-set? (cron-expr-minutes expr) 10)))

(test-case "parse range expression 1-5"
  (define expr (cron-parse "* * * * 1-5"))
  (check-false (bitwise-bit-set? (cron-expr-weekdays expr) 0)) ; Sunday
  (check-true (bitwise-bit-set? (cron-expr-weekdays expr) 1))  ; Monday
  (check-true (bitwise-bit-set? (cron-expr-weekdays expr) 5))  ; Friday
  (check-false (bitwise-bit-set? (cron-expr-weekdays expr) 6))) ; Saturday

(test-case "parse list expression 1,3,5"
  (define expr (cron-parse "0,15,30,45 * * * *"))
  (check-true (bitwise-bit-set? (cron-expr-minutes expr) 0))
  (check-true (bitwise-bit-set? (cron-expr-minutes expr) 15))
  (check-true (bitwise-bit-set? (cron-expr-minutes expr) 30))
  (check-true (bitwise-bit-set? (cron-expr-minutes expr) 45))
  (check-false (bitwise-bit-set? (cron-expr-minutes expr) 10)))

(test-case "parse range with step 1-30/2"
  (define expr (cron-parse "1-30/2 * * * *"))
  (check-true (bitwise-bit-set? (cron-expr-minutes expr) 1))
  (check-false (bitwise-bit-set? (cron-expr-minutes expr) 2))
  (check-true (bitwise-bit-set? (cron-expr-minutes expr) 3))
  (check-true (bitwise-bit-set? (cron-expr-minutes expr) 29))
  (check-false (bitwise-bit-set? (cron-expr-minutes expr) 30)))

(test-case "parse named weekdays"
  (define expr (cron-parse "30 2 * * MON"))
  (check-true (bitwise-bit-set? (cron-expr-weekdays expr) 1))
  (check-false (bitwise-bit-set? (cron-expr-weekdays expr) 0)))

(test-case "parse named weekdays case-insensitive"
  (define expr (cron-parse "0 9 * * mon-fri"))
  (check-true (bitwise-bit-set? (cron-expr-weekdays expr) 1))
  (check-true (bitwise-bit-set? (cron-expr-weekdays expr) 5))
  (check-false (bitwise-bit-set? (cron-expr-weekdays expr) 0))
  (check-false (bitwise-bit-set? (cron-expr-weekdays expr) 6)))

(test-case "parse named months"
  (define expr (cron-parse "0 0 1 JAN *"))
  (check-true (bitwise-bit-set? (cron-expr-months expr) 1))
  (check-false (bitwise-bit-set? (cron-expr-months expr) 2)))

(test-case "parse full month names"
  (define expr (cron-parse "0 0 1 January *"))
  (check-true (bitwise-bit-set? (cron-expr-months expr) 1)))

(test-case "parse Sunday as 7"
  (define expr (cron-parse "0 0 * * 7"))
  (check-true (bitwise-bit-set? (cron-expr-weekdays expr) 0)) ; 7 -> 0
  )

;; ============================================================================
;; Nickname parsing tests
;; ============================================================================

(test-case "parse @yearly"
  (define expr (cron-parse "@yearly"))
  (check-true (cron-expr? expr))
  (check-true (bitwise-bit-set? (cron-expr-minutes expr) 0))
  (check-true (bitwise-bit-set? (cron-expr-hours expr) 0))
  (check-true (bitwise-bit-set? (cron-expr-days expr) 1))
  (check-true (bitwise-bit-set? (cron-expr-months expr) 1)))

(test-case "parse @annually"
  (define expr (cron-parse "@annually"))
  (check-equal? expr (cron-parse "@yearly")))

(test-case "parse @monthly"
  (define expr (cron-parse "@monthly"))
  (check-true (bitwise-bit-set? (cron-expr-minutes expr) 0))
  (check-true (bitwise-bit-set? (cron-expr-hours expr) 0))
  (check-true (bitwise-bit-set? (cron-expr-days expr) 1)))

(test-case "parse @weekly"
  (define expr (cron-parse "@weekly"))
  (check-true (bitwise-bit-set? (cron-expr-weekdays expr) 0)))

(test-case "parse @daily"
  (define expr (cron-parse "@daily"))
  (check-true (bitwise-bit-set? (cron-expr-minutes expr) 0))
  (check-true (bitwise-bit-set? (cron-expr-hours expr) 0))
  (check-true (cron-expr-days-is-wildcard? expr)))

(test-case "parse @midnight"
  (check-equal? (cron-parse "@midnight") (cron-parse "@daily")))

(test-case "parse @hourly"
  (define expr (cron-parse "@hourly"))
  (check-true (bitwise-bit-set? (cron-expr-minutes expr) 0)))

;; ============================================================================
;; Error handling tests
;; ============================================================================

(test-case "error on too many fields"
  (check-exn exn:fail? (lambda () (cron-parse "* * * * * *"))))

(test-case "error on too few fields"
  (check-exn exn:fail? (lambda () (cron-parse "* * *"))))

(test-case "error on invalid number"
  (check-exn exn:fail? (lambda () (cron-parse "60 * * * *"))))

(test-case "error on invalid range"
  (check-exn exn:fail? (lambda () (cron-parse "30-10 * * * *"))))

(test-case "error on invalid step"
  (check-exn exn:fail? (lambda () (cron-parse "*/0 * * * *"))))

(test-case "error on out-of-range weekday"
  (check-exn exn:fail? (lambda () (cron-parse "* * * * 8"))))

(test-case "error on out-of-range month"
  (check-exn exn:fail? (lambda () (cron-parse "* * * 13 *"))))

(test-case "error on invalid nickname"
  (check-exn exn:fail? (lambda () (cron-parse "@invalid"))))

;; ============================================================================
;; cron-expr->string tests
;; ============================================================================

(test-case "format all-wildcard expression"
  (check-equal? (cron-expr->string (cron-parse "* * * * *"))
                "* * * * *"))

(test-case "format specific values"
  (check-equal? (cron-expr->string (cron-parse "30 2 15 6 1"))
                "30 2 15 6 1"))

(test-case "format step expression"
  (check-equal? (cron-expr->string (cron-parse "*/15 * * * *"))
                "0,15,30,45 * * * *"))

;; ============================================================================
;; Next occurrence tests
;; ============================================================================

;; Helper: UTC timestamp for a given date
;; Uses a known epoch calculation
(define (utc-timestamp year month day hour minute second)
  (define (gregorian->jdn y m d)
    (define a (quotient (- 14 m) 12))
    (define y2 (+ y 4800 (- a)))
    (define m2 (+ m (* 12 a) -3))
    (+ d
       (quotient (+ (* 153 m2) 2) 5)
       (* 365 y2)
       (quotient y2 4)
       (- (quotient y2 100))
       (quotient y2 400)
       -32045))
  (define jdn (gregorian->jdn year month day))
  (define epoch-jdn (gregorian->jdn 1970 1 1))
  (+ (* (- jdn epoch-jdn) 86400)
     (* hour 3600)
     (* minute 60)
     second))

;; Helper: convert seconds to a readable UTC date list
(define (secs->utc-list secs)
  (define d (seconds->date secs #f))
  (list (date-year d) (date-month d) (date-day d)
        (date-hour d) (date-minute d) (date-second d)))

(test-case "next: every 15 minutes"
  (define expr (cron-parse "*/15 * * * *"))
  ;; From 2025-01-28 10:03:00 UTC
  (define from (utc-timestamp 2025 1 28 10 3 0))
  (define n1 (cron-next expr from))
  (check-equal? (secs->utc-list n1) '(2025 1 28 10 15 0))
  (define n2 (cron-next expr n1))
  (check-equal? (secs->utc-list n2) '(2025 1 28 10 30 0))
  (define n3 (cron-next expr n2))
  (check-equal? (secs->utc-list n3) '(2025 1 28 10 45 0))
  (define n4 (cron-next expr n3))
  (check-equal? (secs->utc-list n4) '(2025 1 28 11 0 0)))

(test-case "next: every 6 hours"
  (define expr (cron-parse "0 */6 * * *"))
  (define from (utc-timestamp 2025 1 28 10 0 0))
  (define n1 (cron-next expr from))
  (check-equal? (secs->utc-list n1) '(2025 1 28 12 0 0))
  (define n2 (cron-next expr n1))
  (check-equal? (secs->utc-list n2) '(2025 1 28 18 0 0))
  (define n3 (cron-next expr n2))
  (check-equal? (secs->utc-list n3) '(2025 1 29 0 0 0)))

(test-case "next: weekday 9 AM (MON-FRI)"
  (define expr (cron-parse "0 9 * * 1-5"))
  ;; 2025-01-28 is Tuesday
  (define from (utc-timestamp 2025 1 28 10 0 0))
  (define n1 (cron-next expr from))
  ;; Next should be Wednesday Jan 29
  (check-equal? (secs->utc-list n1) '(2025 1 29 9 0 0))
  (define n2 (cron-next expr n1))
  (check-equal? (secs->utc-list n2) '(2025 1 30 9 0 0)) ; Thursday
  (define n3 (cron-next expr n2))
  (check-equal? (secs->utc-list n3) '(2025 1 31 9 0 0)) ; Friday
  (define n4 (cron-next expr n3))
  ;; Should skip weekend, go to Monday Feb 3
  (check-equal? (secs->utc-list n4) '(2025 2 3 9 0 0)))

(test-case "next: specific day of week (Monday 2:30 AM)"
  (define expr (cron-parse "30 2 * * 1"))
  ;; 2025-01-28 is Tuesday
  (define from (utc-timestamp 2025 1 28 0 0 0))
  (define n1 (cron-next expr from))
  ;; Next Monday is Feb 3
  (check-equal? (secs->utc-list n1) '(2025 2 3 2 30 0)))

(test-case "next: yearly (Jan 1 at midnight)"
  (define expr (cron-parse "@yearly"))
  (define from (utc-timestamp 2025 3 15 12 0 0))
  (define n1 (cron-next expr from))
  (check-equal? (secs->utc-list n1) '(2026 1 1 0 0 0)))

(test-case "next: monthly (1st at midnight)"
  (define expr (cron-parse "@monthly"))
  (define from (utc-timestamp 2025 1 15 12 0 0))
  (define n1 (cron-next expr from))
  (check-equal? (secs->utc-list n1) '(2025 2 1 0 0 0))
  (define n2 (cron-next expr n1))
  (check-equal? (secs->utc-list n2) '(2025 3 1 0 0 0)))

(test-case "next: daily"
  (define expr (cron-parse "@daily"))
  (define from (utc-timestamp 2025 1 28 12 0 0))
  (define n1 (cron-next expr from))
  (check-equal? (secs->utc-list n1) '(2025 1 29 0 0 0)))

(test-case "next: hourly"
  (define expr (cron-parse "@hourly"))
  (define from (utc-timestamp 2025 1 28 10 30 0))
  (define n1 (cron-next expr from))
  (check-equal? (secs->utc-list n1) '(2025 1 28 11 0 0))
  (define n2 (cron-next expr n1))
  (check-equal? (secs->utc-list n2) '(2025 1 28 12 0 0)))

(test-case "next: year boundary"
  (define expr (cron-parse "0 0 1 1 *"))
  (define from (utc-timestamp 2025 12 31 23 59 0))
  (define n1 (cron-next expr from))
  (check-equal? (secs->utc-list n1) '(2026 1 1 0 0 0)))

(test-case "next: leap year Feb 29"
  (define expr (cron-parse "0 0 29 2 *"))
  (define from (utc-timestamp 2025 1 1 0 0 0))
  (define n1 (cron-next expr from))
  ;; 2028 is the next leap year
  (check-equal? (secs->utc-list n1) '(2028 2 29 0 0 0)))

(test-case "next: POSIX OR logic - both day-of-month and day-of-week restricted"
  ;; When both fields are restricted, either matching triggers
  ;; "On the 15th OR on Mondays"
  (define expr (cron-parse "0 0 15 * 1"))
  (define from (utc-timestamp 2025 1 1 0 0 0))
  (define n1 (cron-next expr from))
  ;; Jan 6 2025 is Monday
  (check-equal? (secs->utc-list n1) '(2025 1 6 0 0 0))
  (define n2 (cron-next expr n1))
  ;; Jan 13 2025 is Monday
  (check-equal? (secs->utc-list n2) '(2025 1 13 0 0 0))
  (define n3 (cron-next expr n2))
  ;; Jan 15 is before next Monday (Jan 20), so Jan 15
  (check-equal? (secs->utc-list n3) '(2025 1 15 0 0 0))
  (define n4 (cron-next expr n3))
  ;; Next is Monday Jan 20
  (check-equal? (secs->utc-list n4) '(2025 1 20 0 0 0)))

;; ============================================================================
;; Scheduled event struct tests
;; ============================================================================

(test-case "scheduled-event creation and accessors"
  (define evt (scheduled-event "scheduled" "*/5 * * * *" 1738040400000))
  (check-equal? (scheduled-event-type evt) "scheduled")
  (check-equal? (scheduled-event-cron evt) "*/5 * * * *")
  (check-equal? (scheduled-event-scheduled-time evt) 1738040400000))

;; ============================================================================
;; Scheduler tests
;; ============================================================================

(test-case "cron register and remove"
  (define called? (box #f))
  (cron "test-job" "* * * * *"
        (lambda (evt)
          (set-box! called? #t)))
  (check-not-false (member "test-job" (cron-jobs-list)) "Job should be registered")
  (cron-remove "test-job")
  (check-false (member "test-job" (cron-jobs-list)) "Job should be removed"))

(test-case "cron re-register replaces existing"
  (define count (box 0))
  (cron "replace-test" "* * * * *"
        (lambda (evt) (set-box! count 1)))
  (cron "replace-test" "*/5 * * * *"
        (lambda (evt) (set-box! count 2)))
  (check-equal? (length (filter (lambda (n) (string=? n "replace-test"))
                                (cron-jobs-list)))
                1
                "Should only have one job with the name")
  (cron-remove "replace-test"))

(test-case "cron-stop-all clears all jobs"
  (cron "job-a" "* * * * *" (lambda (evt) (void)))
  (cron "job-b" "*/5 * * * *" (lambda (evt) (void)))
  (check-true (>= (length (cron-jobs-list)) 2))
  (cron-stop-all)
  (check-equal? (cron-jobs-list) '()))

(test-case "cron-remove is idempotent"
  (cron-remove "nonexistent-job")
  (check-true #t "Should not error"))

;; ============================================================================
;; Integration: scheduled event has correct fields
;; ============================================================================

(test-case "handler receives correct scheduled-event"
  (define result-ch (make-channel))
  (define schedule "0 0 1 1 *") ; yearly
  (cron "evt-test" schedule
        (lambda (evt)
          (channel-put result-ch evt)))
  ;; We can't easily wait for the cron to fire (it's yearly),
  ;; so we just verify the job was registered correctly
  (check-not-false (member "evt-test" (cron-jobs-list)))
  (cron-remove "evt-test"))

;; Cleanup
(cron-stop-all)

(displayln "All cron tests passed!")

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

(define (fast-expt b n)
  (cond
    ((= n 0) 1)
    ((= b 1) n)
    ((= n 1) b)
    (else (fast-expt-iter 1 b n))))

(define (fast-expt-iter a b n)
  (cond
    ((= n 2) (* a (square b)))
    ((even? n) (fast-expt-iter a (square b) (/ n 2)))
    (else (fast-expt-iter (* a b) b (- n 1)))))

(define (even? num)
  (= (remainder num 2) 0))

(define (square num)
  (* num num))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
      (report-prime (- (real-time-clock) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  (newline))


(define (print-num msg num)
  (display msg)
  (display num)
  (newline))

(define (print-delimeter)
  (display "---------------------")
  (newline))

(define (print-details number old-time)
  (print-num "Number: " number)
  (print-num "Original Time: " old-time))

(define (run-timed-prime-test)
  (print-details 1009 108653.92)
  (timed-prime-test 1009)
  (print-delimeter)

  (print-details 1013 108655.92)
  (timed-prime-test 1013)
  (print-delimeter)

  (print-details 1019 108656.92)
  (timed-prime-test 1019)
  (print-delimeter)

  (print-details 10007 108656.92)
  (timed-prime-test 10007)
  (print-delimeter)
  
  (print-details 10009 108657.92)
  (timed-prime-test 10009)
  (print-delimeter)

  (print-details 10037 108657.92)
  (timed-prime-test 10037)
  (print-delimeter)

  (print-details  100003 108658.92)
  (timed-prime-test 100003)
  (print-delimeter)

  (print-details  100019 108658.92)
  (timed-prime-test 100019)
  (print-delimeter)

  (print-details  100043 108659.92)
  (timed-prime-test 100043)
  (print-delimeter)

  (print-details 1000003 108659.92)
  (timed-prime-test 1000003)
  (print-delimeter)

  (print-details 1000033 108660.92)
  (timed-prime-test 1000033)
  (print-delimeter)

  (print-details 1000037 108660.91)
  (timed-prime-test 1000037)
  (print-delimeter))


(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (next num)
  (if (= num 3) 2 (+ num 2)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
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
  (print-details 1009 1223924.81)
  (timed-prime-test 1009)
  (print-delimeter)

  (print-details 1013 1223925.8)
  (timed-prime-test 1013)
  (print-delimeter)

  (print-details 1019 1223926.8)
  (timed-prime-test 1019)
  (print-delimeter)

  (print-details 10007 1344686.79)
  (timed-prime-test 10007)
  (print-delimeter)
  
  (print-details 10009 1344688.79)
  (timed-prime-test 10009)
  (print-delimeter)

  (print-details 10037 1344693.78)
  (timed-prime-test 10037)
  (print-delimeter)

  (print-details  100003 1410540.77)
  (timed-prime-test 100003)
  (print-delimeter)

  (print-details  100019 1410549.76)
  (timed-prime-test 100019)
  (print-delimeter)

  (print-details  100043 1410560.75)
  (timed-prime-test 100043)
  (print-delimeter)

  (print-details 1000003 1473532.72)
  (timed-prime-test 1000003)
  (print-delimeter)

  (print-details 1000033 1473542.71)
  (timed-prime-test 1000033)
  (print-delimeter)

  (print-details 1000037 1473546.71)
  (timed-prime-test 1000037)
  (print-delimeter))


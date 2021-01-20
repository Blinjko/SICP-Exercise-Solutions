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


(define (fast-expt-test b mx)
  (cond
    ((= mx -1) 0)
    (else
      (print-number "Base: " b)
      (print-number "Exponent: " mx)
      (print-number "Result: " (fast-expt b mx))
      (display "---------------------------")
      (newline)
      (fast-expt-test b (- mx 1)))))
  

(define (print-number message num)
  (display message)
  (display num)
  (newline))

(fast-expt-test 10 12)

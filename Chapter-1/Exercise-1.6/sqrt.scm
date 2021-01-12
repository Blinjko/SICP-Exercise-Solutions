(define (sqrt x)
  (if (= x 0)
      0
      (sqrt-iter 0.001 x)))

(define (sqrt-iter guess x)
  (if (close-enough-guess guess x)
      guess
      (sqrt-iter (calculate-guess guess x) x)))

(define (close-enough-guess guess x)
  (if (>= 0.0001 (abs (- x (* guess guess))))
      true
      false))

(define (calculate-guess old-guess x)
  (/ 
    (+ (/ x old-guess) 
       old-guess) 
    2))

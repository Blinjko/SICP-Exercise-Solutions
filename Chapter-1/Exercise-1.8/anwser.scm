(define (cube-root x)
  (if (= x 0)
      0
      (cube-root-iter -1.0 1.0 x)))

(define (cube-root-iter last-guess current-guess x)
  (if (close-enough-guess last-guess current-guess x)
      current-guess
      (cube-root-iter current-guess (calculate-guess current-guess x) x)))

(define (close-enough-guess last-guess current-guess x)
  (if (>= 0.0001 (abs (- current-guess last-guess)))
      true
      false))

(define (calculate-guess old-guess x)
  (/ 
    (+ 
      (/ x 
         (* old-guess 
            old-guess)) 
      (* 2
         old-guess))
    3))

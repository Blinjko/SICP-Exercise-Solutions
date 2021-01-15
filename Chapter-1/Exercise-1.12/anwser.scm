(define (pascals-triangle y x)
  (cond ((> x y) -1)
        ((or (= y 1) (= y 2)) 1)
        ((or (= x 1) (= y x)) 1)
        (else (+ 
                (pascals-triangle (- y 1) (- x 1))
                (pascals-triangle (- y 1) x)))))

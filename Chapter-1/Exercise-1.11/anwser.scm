
(define (f-recursive n)
  (if (> 3 n)
      n
      (+ 
        (f-recursive (- n 1))
        (* 2 (f-recursive (- n 2)))
        (* 3 (f-recursive (- n 3))))))


(define (f n)
  (define (f-iter counter f1 f2 f3)
    (define (calculate-f)
      (+ 
        f1
        (* 2 f2)
        (* 3 f3)))

    (if (= 0 counter)
        (calculate-f)
        (f-iter (- counter 1)
                (calculate-f)
                f1
                f2)))
  (if (> 3 n)
      n
      (f-iter (- n 3) 2 1 0)))

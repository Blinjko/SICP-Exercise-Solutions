(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

(define (congruent n)
  (congruent-iter n (- n 1)))

(define (congruent-iter n a)
  (cond 
    ((= 0 a))
    (else (display n)
          (display " congruent to ")
          (display a)
          (display " = ")
          (display (= (expmod a n n) (remainder a n)))
          (newline)
          (congruent-iter n (- a 1)))))

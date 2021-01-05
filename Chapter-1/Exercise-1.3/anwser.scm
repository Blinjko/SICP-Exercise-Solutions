(define (square x) (* x x))

(define (proc a b c)
  (cond ((and (< c a) (< c b)) (+ (square a) (square b)))
        ((and (< a b) (< a c)) (+ (square b) (square c)))
        (else (+ (square a) (square c)))))

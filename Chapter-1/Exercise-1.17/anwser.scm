(define (mult a b)
  (cond
    ((or (= a 0) (= b 0)) 0)
    ((= a 1) b)
    ((= b 1) a)
    (else (mult-iter 0 a b))))

(define (mult-iter total a b)
  (cond
    ((= b 2) (+ total (double a)))
    ((even? b) (mult-iter total (double a) (halve b)))
    (else (mult-iter (+ total a) a (- b 1)))))

(define (even? n)
  (= 0 (remainder n 2)))

(define (double n)
  (+ n n))

(define (halve n)
  (/ n 2))

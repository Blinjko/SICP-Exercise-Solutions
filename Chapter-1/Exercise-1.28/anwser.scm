(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (non-trivial-square-root (expmod base (/ exp 2) m) m))
                    m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

(define (non-trivial-square-root number n)
  (if (and (not (= number 1)) (not (= number (- n 1))) (= (square number) (remainder 1 n)))
      0
      number))

(define (miller-rabin-test n)
  (define (signal-check x)
    (if (= 0 x)
        false
        (= x (remainder 1 n))))

  (define (try-it a)
    (signal-check (expmod a (- n 1) n)))
  (try-it (+ 1 (random (- n 1)))))


(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else false)))

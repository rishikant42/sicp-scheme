(define (integer-starting-from n)
  (cons-stream n (integer-starting-from (+ n 1))))

(define integers (integer-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes))

(define primes (cons-stream 2
                            (stream-filter prime? (integer-starting-from 3))))

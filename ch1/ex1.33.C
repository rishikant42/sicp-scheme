(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n divisor)
  (cond ((> (square divisor) n) n)
        ((devides? n divisor) divisor)
        (else (find-divisor n (+ divisor 1)))))

(define (devides? n divisor)
  (= (remainder n divisor) 0))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

;; (define (relative-prime? a b)
;;   (= (gcd a b) 1))

(define (identity x) x )
(define (inc x) (+ x 1))

(define (filtered-accumulate combiner null-value term a next b filter)
  (if (> a b)
    null-value
    (if (filter a)
      (combiner (term a) (filtered-accumulate combiner null-value term (next a) next b filter))
      (combiner null-value (filtered-accumulate combiner null-value term (next a) next b filter)))))

(define (product term a next b filter)
  (filtered-accumulate * 1 term a next b filter))

(define (int-filter a)
  (= a a))

(define (product-integers a b)
  (product identity a inc b int-filter))

(define (product-primes a b)
  (product identity a inc b prime?))

(define (product-relative-prime a b)
  (define (relative-prime? x)
    (= (gcd x b) 1))
  (product identity a inc b relative-prime?))

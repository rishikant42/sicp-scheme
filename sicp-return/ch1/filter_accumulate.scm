(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (if (< n 2)
    false
    (= (smallest-divisor n) n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (identity x) x)
(define (inc x) (+ x 1))

(define (filtered-accumulate combiner null-value term a next b predicate)
  (if (> a b)
    null-value
    (if (predicate a)
      (combiner (term a) (filtered-accumulate combiner null-value term (next a) next b predicate))
      (filtered-accumulate combiner null-value term (next a) next b predicate))))

(define (sum term a next b predicate)
  (filtered-accumulate + 0 term a next b predicate))

(define (prime-sum a b)
  (sum identity a inc b prime?))

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (identity x) x)
(define (inc n) (+ n 1))

(define (sum-integers a b)
  (sum identity a inc b))

(define (sum-square a b)
  (sum square a inc b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (product-integers a b)
  (product identity a inc b))

(define (product-square a b)
  (product square a inc b))

(define (factorial n)
  (product identity 1 inc n))

;; Above accumulate recursive procedure will genrate iterative process

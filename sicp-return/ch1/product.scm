(define (product term a next b)
  (if (> a b)
    1
    (* (term a)
       (product term (next a) next b))))

(define (identity x) x)
(define (square x ) (* x x))
(define (inc x) (+ x 1))

(define (product-integers a b)
  (product identity a inc b))

(define (product-squares a b)
  (product square a inc b))

(define (pi-product a b)
  (define (pi-term x)
    (if (even? x)
      (/ (+ x 2) (+ x 1))
      (/ (+ x 1) (+ x 2))))
  (product pi-term a inc b))

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (identity x) x)

(define (inc x) (+ x 1))

(define (cube x) (* x x x))

(define (sum-integers a b)
  (sum identity a inc b))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (pi-term x) (/ 1.0 (* x (+ x 2))))

(define (pi-next x) (+ x 4))

(define (pi-sum a b)
  (sum pi-term a pi-next b))

(define (identity x) x)

(define (cube x) (* x x x))

(define (pi-term x) (/ 1.0 (* x (+ x 2))))

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (integral f a b dx)
  (define ft (+ a (/ dx 2.0)))
  (define (add-dx x) (+ x dx))
  (* (sum f ft add-dx b)
     dx))


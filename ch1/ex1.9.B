(define (inc n)
  (+ n 1))

(define (dec n)
  (- n 1))

(define (+ a b)
  (if (= a 0)
    b
    (+ (dec a) (inc b))))

;; Recusive procedure --> Iterative process ==> tail recusive

;; => (+ 4 5)
;; => (+ 3 6)
;; => (+ 2 7)
;; => (+ 1 8)
;; => (+ 0 9)
;; => 9

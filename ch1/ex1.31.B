(define (inc n) (+ n 1))

;; (define (product-integers a b)
;;   (define (iter a result)
;;     (if (> a b)
;;       result
;;       (iter (inc a) (* result a))))
;;   (iter a 1))
;; 
;; (define (product-sqr a b)
;;   (define (iter a result)
;;     (if (> a b)
;;       result
;;       (iter (inc a) (* result (square a)))))
;;   (iter a 1))
;; 
;; (define (pi-product a b)
;;   (define (iter a result)
;;     (if (> a b)
;;       result
;;       (if (even? a)
;;         (iter (inc a) (* result (/ (+ a 2) (+ a 1))))
;;         (iter (inc a) (* result (/ (+ a 1) (+ a 2)))))))
;;   (iter a 1))

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))))
  (iter a 1))

(define (identity x) x)

(define (product-integers a b)
  (product identity a inc b))

(define (product-sqr a b)
  (product square a inc b))

(define (pi-term x)
  (if (even? x)
    (/ (+ x 2) (+ x 1))
    (/ (+ x 1) (+ x 2))))

(define (pi-product a b)
  (product pi-term a inc b))

;; Above recursive procedure generate iterative processs

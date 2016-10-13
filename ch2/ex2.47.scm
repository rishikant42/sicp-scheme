;; (define (make-frame origin edge1 edge2)
;;   (list origin edge1 edge2))
;; 
;; (define (origin-frame f) (car f))
;; (define (edge1-frame f)  (cadr f))
;; (define (edge2-frame f) (caddr f))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame f) (car f))
(define (edge1-frame f) (cadr f))
(define (edge2-frame f) (cddr f))

(define (make-vect x y)
  (cons x y))

(define o (make-vect 0 0))
(define e1 (make-vect 1 2))
(define e2 (make-vect 3 4))

;; (define f (make-frame o e1 e2))
;; (display f)
;; (display (origin-frame f))
;; (display (edge1-frame f))
;; (display (edge2-frame f))

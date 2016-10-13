#lang sicp
(#%require sicp-pict)

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
      (origin-frame frame)
      (add-vect (scale-vect (xcor-vect v)
                            (edge1-frame frame))
                (scale-vect (ycor-vect v)
                            (edge2-frame frame))))))


(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1)
                (xcor-vect v2))
             (+ (ycor-vect v1)
                (ycor-vect v2))))

(define (scale-vect factor v)
  (make-vect (* (xcor-vect v) factor)
             (* (ycor-vect v) factor)))

;; (define (make-frame origin edge1 edge2)
;;   (cons origin (cons edge1 edge2)))
;; 
;; (define (origin-frame f) (car f))
;; (define (edge1-frame f) (cadr f))
;; (define (edge2-frame f) (cddr f))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame f) (car f))
(define (edge1-frame f)  (cadr f))
(define (edge2-frame f) (caddr f))

(define o (make-vect 0 0))
(define e1 (make-vect 3 4))
(define e2 (make-vect 5 6))
(define a-frame (make-frame o e1 e2))
(display ((frame-coord-map a-frame) (make-vect 2 3)))

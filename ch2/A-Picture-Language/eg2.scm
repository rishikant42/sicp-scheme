#lang sicp
(#%require sicp-pict)

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (identity x) x)

(define (flipped-pair painter)
  (let ((combine4 (square-of-four identity flip-vert identity flip-vert)))
    (combine4 painter)))

;; (paint (flipped-pair einstein))

(define (split p1 p2)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split p1 p2) painter (- n 1))))
          (p1 painter (p2 smaller smaller))))))

;; (define (up-split painter n)
;;   ((split below beside) painter n))

;;;;;;;;;;    or ;;;;;;;;;;

(define up-split (split below beside))

;; (paint (up-split einstein 2))

;; (define (right-split painter n)
;;   ((split beside below) painter n))

;;;;;;;;;;;  or ;;;;;;;;;;;;;;;;;;;;;

(define right-split (split beside below))
;; (paint (right-split einstein 2))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (corner (corner-split painter (- n 1)))
              (bottom-right (below right right)))
          (beside (below painter top-left) (below bottom-right corner))))))

(define (rotate180 painter)
  (flip-vert (flip-horiz painter)))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

;; (paint (square-limit einstein 1))

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

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1)
                (xcor-vect v2))
             (- (ycor-vect v1)
                (ycor-vect v2))))

(define (scale-vect factor v)
  (make-vect (* (xcor-vect v) factor)
             (* (ycor-vect v) factor)))

;; (define (make-frame origin edge1 edge2)
;;    (list origin edge1 edge2))
;; 
;; (define (origin-frame f)
;;   (car f))
;; 
;; (define (edge1-frame f)
;;   (cadr f))
;; 
;; (define (edge2-frame f)
;;   (caddr f))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame f)
  (car f))

(define (edge1-frame f)
  (cadr f))

(define (edge2-frame f)
  (cddr f))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect (origin-frame frame)
              (add-vect (scale-vect (xcor-vect v)
                                    (edge1-frame frame))
                        (scale-vect (ycor-vect v)
                                    (edge2-frame frame))))))

(define o  (make-vect 0 0))
(define e1 (make-vect 1 2))
(define e2 (make-vect 3 4))
(define f (make-frame o e1 e2))
;; (display ((frame-coord-map f) (make-vect 10 4)))

(define (make-segment v1 v2)
  (cons v1 v2))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (for-each proc items)
  (if (not (null? items))
    (proc (car items)))
  (if (null? items)
    true
    (for-each proc (cdr items))))

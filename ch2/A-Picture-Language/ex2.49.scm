#lang racket
(require (lib "racket/draw"))
(require racket/class)

(define target (make-bitmap 100 100))
(define dc (new bitmap-dc% [bitmap target]))

(define (make-vect x y)
  (cons x y))

(define (xcor-vect vect)
  (car vect))

(define (ycor-vect vect)
  (cdr vect))

(define (add-vect a b)
  (make-vect (+ (xcor-vect a) (xcor-vect b))
             (+ (ycor-vect a) (ycor-vect b))))

(define (sub-vect a b)
  (make-vect (- (xcor-vect a) (xcor-vect b))
             (- (ycor-vect a) (ycor-vect b))))

(define (scale-vect s a)
  (make-vect (* (xcor-vect a) s)
             (* (ycor-vect a) s)))

;; Frames

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (caddr frame))

;; Coords mapping

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect 
      (origin-frame frame)
      (add-vect (scale-vect (xcor-vect v)
                            (edge1-frame frame))
                (scale-vect (ycor-vect v)
                            (edge2-frame frame))))))

;; Segments

(define (make-segment v1 v2)
  (cons v1 v2))

(define (start-segment v1)
  (car v1))

(define (end-segment v2)
  (cdr v2))

;; Painters

(define (my-draw-line v1 v2)
  (send dc draw-line
        (xcor-vect v1) (ycor-vect v1)
        (xcor-vect v2) (ycor-vect v2)))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
        (my-draw-line
          ((frame-coord-map frame) (start-segment segment))
          ((frame-coord-map frame) (end-segment segment))))
      segment-list)))

;; Painting
;; a.

(define s1 (make-segment (make-vect 0 0) (make-vect 0 1)))
(define s2 (make-segment (make-vect 0 1) (make-vect 1 1)))
(define s3 (make-segment (make-vect 1 1) (make-vect 1 0)))
(define s4 (make-segment (make-vect 1 0) (make-vect 0 0)))

(define segment-list1 (list s1 s2 s3 s4))

(define (a-painter frame)
  ((segments->painter segment-list1) frame))

;;;;;;;;;;;;;;;; or ;;;;;;;;;;;;;;;;

;; (define a-painter (segments->painter 
;;                     (list (make-segment 
;;                             (make-vect 0 0) 
;;                             (make-vect 0 1))
;;                           (make-segment 
;;                             (make-vect 0 1) 
;;                             (make-vect 1 1))
;;                           (make-segment
;;                             (make-vect 1 1)
;;                             (make-vect 1 0))
;;                           (make-segment
;;                             (make-vect 1 0)
;;                             (make-vect 0 0)))))
;; 

(send dc clear)
(a-painter (make-frame (make-vect 0 0) (make-vect 0 99) (make-vect 99 0)))
(send target save-file "ex2-49-a.png" 'png)

;; b.

(define s5 (make-segment (make-vect 0 0) (make-vect 1 1)))
(define s6 (make-segment (make-vect 0 1) (make-vect 1 0)))

(define segment-list2 (list s5 s6))

(define (b-painter frame)
  ((segments->painter segment-list2) frame))

;;;;;;;;;;; or ;;;;;;;;;;;;;;;;
;;
;; (define b-painter (segments->painter 
;;                     (list (make-segment 
;;                             (make-vect 0 0) 
;;                             (make-vect 1 1))
;;                           (make-segment 
;;                             (make-vect 0 1) 
;;                             (make-vect 1 0)))))
 
(send dc clear)
(b-painter (make-frame (make-vect 0 0) (make-vect 0 100) (make-vect 100 0)))
(send target save-file "ex2-49-b.png" 'png)

;; c

(define s7 (make-segment (make-vect 0 0.5) (make-vect 0.5 0)))
(define s8 (make-segment (make-vect 0.5 0) (make-vect 1 0.5)))
(define s9 (make-segment (make-vect 1 0.5) (make-vect 0.5 1)))
(define s0 (make-segment (make-vect 0.5 1) (make-vect 0 0.5)))

(define segment-list3 (list s7 s8 s9 s0))

(define (c-painter frame)
  ((segments->painter segment-list3) frame))

;;;;;;;;;;;;;;;;;;;; or ;;;;;;;;;;;;;;

;; (define c-painter (segments->painter 
;;                     (list (make-segment 
;;                             (make-vect 0 0.5) 
;;                             (make-vect 0.5 0))
;;                           (make-segment 
;;                             (make-vect 0.5 0) 
;;                             (make-vect 1 0.5))
;;                           (make-segment
;;                             (make-vect 1 0.5)
;;                             (make-vect 0.5 1))
;;                           (make-segment
;;                             (make-vect 0.5 1)
;;                             (make-vect 0 0.5)))))

(send dc clear)
(c-painter (make-frame (make-vect 0 0) (make-vect 0 100) (make-vect 100 0)))
(send target save-file "ex2-49-c.png" 'png)

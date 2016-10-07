#lang sicp
(#%require sicp-pict)

(define (right-split1 painter n)
  (if (= n 0)
      painter
      (beside painter
              (right-split1 (below painter painter) (- n 1)))))

;; (paint (right-split1 einstein 3))

(define (right-split2 painter n)
  (if (= n 0)
      painter
      (let ((split (right-split2 (below painter painter) (- n 1))))
        (beside painter split))))

;; (paint (right-split2 einstein 3))

(define (right-split3 painter n)
  (if (= n 0)
      painter
      (beside painter
              (below (right-split3 painter (- n 1)) (right-split3 painter (- n 1))))))

;; (paint (right-split3 einstein 3))

(define (right-split4 painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split4 painter (- n 1))))
        (beside painter (below smaller smaller)))))

(paint (right-split4 einstein 3))
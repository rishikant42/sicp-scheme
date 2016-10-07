#lang sicp
(#%require sicp-pict)

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split1 painter n)
  (if (= n 0)
      painter
      (let ((left (below painter (beside (up-split painter (- n 1)) (up-split painter (- n 1)))))
            (right (below (below (right-split painter (- n 1)) (right-split painter (- n 1))) (corner-split1 painter (- n 1)))))
        (beside left right))))

(define (corner-split2 painter n)
  (if (= n 0)
    painter
    (let ((up (up-split painter (- n 1)))
          (right (right-split painter (- n 1))))
      (let ((top-left (beside up up))
            (bottom-right (below right right))
            (corner (corner-split2 painter (- n 1))))
        (beside (below painter top-left)
                (below bottom-right corner))))))

(paint (corner-split2 einstein 2))

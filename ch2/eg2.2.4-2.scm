#lang sicp
(#%require sicp-pict)

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define eins4 (flipped-pairs einstein))

(paint eins4)

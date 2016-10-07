#lang sicp
(#%require sicp-pict)
(define paint2 (beside paint (flip-vert paint)))
(define paint4 (below paint2 paint2))

(paint einstein)
(paint (beside einstein einstein))
(paint (below einstein einstein))
(paint (flip-vert einstein))
(paint (flip-horiz einstein))

(paint (flip-vert  einstein))
(paint  (beside  einstein (flip-vert  einstein)))
(paint (below (beside  einstein (flip-vert  einstein))
              (beside  einstein (flip-vert  einstein))))

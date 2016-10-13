#lang sicp
(#%require sicp-pict)

(define eins2 (beside einstein (flip-vert einstein)))
(define eins4 (below eins2 eins2))

;; (paint eins2)
;; (paint eins4)

(define (flipped-pair painter)
  (let ((smaller (beside painter (flip-vert painter))))
    (below smaller smaller)))

;; (paint (flipped-pair einstein))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

;; (paint (right-split einstein 2))

(define (left-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (left-split painter (- n 1))))
        (beside (below smaller smaller) painter))))

;; (paint (left-split einstein 2))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;; (paint (up-split einstein 2))

(define (down-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (down-split painter (- n 1))))
        (below (beside smaller smaller) painter))))

;; (paint (down-split einstein 2))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (corner (corner-split painter (- n 1)))
              (bottom-right (below right right)))
          (beside (below painter top-left) (below bottom-right corner))))))

;; (paint (corner-split einstein 2))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

;; (paint (square-limit einstein 1))

;; (define (sqr-limit painter n)
;;   (let ((q (corner-split painter n)))
;;     (let ((h (below (flip-vert q) q)))
;;       (beside (flip-horiz h) h))))
;; 
;; (paint (sqr-limit einstein 1))

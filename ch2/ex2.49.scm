(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
        (draw-line
          ((frame-coord-map frame) (start-segment segment))
          ((frame-coord-map frame) (end-segment segment))))
      segment-list)))

(define (make-segment v1 v2)
  (cons v1 v2))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (make-vect x y)
  (cons x y))

(let ((tl (make-vect 0 1)) 
      (tr (make-vect 1 1)) 
      (bl (make-vect 0 0)) 
      (br (make-vect 1 0))) 
  ;; a 
  (segments->painter (list 
                       (make-segment bl tl) 
                       (make-segment tl tr) 
                       (make-segment tr br) 
                       (make-segment br bl))))

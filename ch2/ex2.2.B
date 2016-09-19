(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment p)
  (car p))

(define (end-segment p)
  (cdr p))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (mid-point p1 p2)
  (make-point (/ (+ (x-point p1) (x-point p2)) 2)
              (/ (+ (y-point p1) (y-point p2)) 2)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")"))


;; 1 ]=> (define a (make-point 2 4))
;; 
;; ;Value: a
;; 
;; 1 ]=> (define b (make-point 6 8))
;; 
;; ;Value: b
;; 
;; 1 ]=> (print-point (mid-point a b))
;; 
;; (4, 6)

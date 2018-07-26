(define (make-point x y) (cons x y))

(define (x-point p) (car p))

(define (y-point p) (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")"))

(define (average a b) (/ (+ a b) 2.0))

(define (make-segment p1 p2) (cons p1 p2))

(define (start-segment s) (car s))

(define (end-segment s) (cdr s))

(define (mid-point s)
  (let ((p1 (start-segment s))
        (p2 (end-segment s)))
    (make-point (average (x-point p1) (x-point p2))
                (average (y-point p1) (y-point p2)))))

(define (distance p1 p2)
  (sqrt (+ (square (- (x-point p1) (x-point p2)))
           (square (- (y-point p1) (y-point p2))))))

(define (rectangle s1 s2) (cons s1 s2))

(define (first-segment r) (car r))

(define (second-segment r) (cdr r))

(define (area r)
  (let ((s1 (first-segment r))
        (s2 (second-segment r)))
    (let ((d1 (distance (start-segment s1) (end-segment s1)))
          (d2 (distance (start-segment s2) (end-segment s2))))
      (* d1 d2))))

(define (perimeter r)
  (let ((s1 (first-segment r))
        (s2 (second-segment r)))
    (let ((d1 (distance (start-segment s1) (end-segment s1)))
          (d2 (distance (start-segment s2) (end-segment s2))))
      (* 2 (+ d1 d2)))))

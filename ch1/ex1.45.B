(define tolerance 0.00001)
(define (average a b) (/ (+ a b) 2.0))

(define (close-enough? a b)
  (< (abs (- a b)) tolerance))

(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(define (newton-transform g)
  (lambda (x) (- x 
                 (/ (g x)
                    ((deriv g) x)))))

(define (average-damping f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damping (lambda (y) (/ x y)))
               1.0))

(define (cube-root x)
  (fixed-point (average-damping (lambda (y) (/ x (square y))))
               1.0))

(define (cube x) (* x x x))

(define (quad-root x)
  (fixed-point (average-damping (average-damping (lambda (y) (/ x (cube y)))))
               1.0))

(define (quad x) (* x x x x))

(define (penta-root x)
  (fixed-point (average-damping (average-damping (lambda (y) (/ x (quad y)))))
               1.0))

(define (penta x) (* x x x x x))

(define (hexa-root x)
  (fixed-point (average-damping (average-damping (lambda (y) (/ x (penta y)))))
               1.0))

(define (hexa x) (* x x x x x x))

(define (svn-root x)
  (fixed-point (average-damping (average-damping (lambda (y) (/ x (hexa y)))))
               1.0))

(define (svn x) (* x x x x x x x))

(define (egh-root x)
  (fixed-point (average-damping (average-damping (average-damping (lambda (y) (/ x (svn y))))))
               1.0))

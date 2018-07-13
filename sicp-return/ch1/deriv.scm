(define tolerance 0.00001)

(define (close-enough? a b)
  (< (abs (- a b)) tolerance))

(define (fixed-point f init-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try init-guess))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define dx 0.00001)

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x))
                 dx)))

(define (newton-transform g)
  (lambda (x) (- x
                 (/ (g x)
                    ((deriv g) x)))))

(define (newton-method g guess)
  (fixed-point (newton-transform g)
               guess))

(define (sqrt x)
  (newton-method (lambda (guess) (- (square guess) x))
                 1.0))

(define (cube x) (* x x x))

(define (cube-root x)
  (newton-method (lambda (guess) (- (cube guess) x))
                 1.0))

(define (nth-root x n)
  (newton-method (lambda (guess) (- (expt guess n) x))
                 1.0))

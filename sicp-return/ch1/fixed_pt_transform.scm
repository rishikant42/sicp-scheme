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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (average a b) (/ (+ a b) 2.0))

(define (average-damp f)
  (lambda (x) (average x (f x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt1 x)
  (fixed-point-of-transform (lambda (guess) (- (square guess) x))
                            newton-transform
                            1.0))

(define (sqrt2 x)
  (fixed-point-of-transform (lambda (guess) (/ x guess))
                            average-damp
                            1.0))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (nth-root x n)
  (fixed-point-of-transform (lambda (guess) (- (expt guess n) x))
                            newton-transform
                            1.0))

(define (nth-root2 x n)
  (fixed-point-of-transform (lambda (guess) (/ x (expt guess (- n 1))))
                            average-damp
                            1.0))

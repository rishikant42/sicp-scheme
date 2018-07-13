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

(define phi (fixed-point (lambda(x) (+ 1 (/ 1 x)))
                         1.0))

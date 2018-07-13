(define (average a b) (/ (+ a b) 2.0))

(define tolerance 0.00001)

(define (close-enough? a b)
  (< (abs (- a b)) tolerance))

(define (fixed-point f init-guess)
  (define (try guess)
    (newline)
    (display "Guess: ")
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try init-guess))

(define (xx1 y) (fixed-point (lambda(x) (/ (log y) (log x)))
                        1.1))

(define (xx2 y) (fixed-point (lambda(x) (average x (/ (log y) (log x))))
                        1.1))

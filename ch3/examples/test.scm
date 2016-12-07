(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
    (begin (set! balance (- balance amount))
           balance)
    "Insufficient funds"))

(define (test x)
  (cond ((= x 1) (display (+ x 5)) (display (+ x 10)))
        ((= x 2) (display (* x 5)) (display (* x 10)))
        ((= x 3) (display (- x 5)) (display (- x 10)))))

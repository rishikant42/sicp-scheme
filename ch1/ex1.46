(define (close-enough? a b)
  (< (abs (- a b)) 0.00001))

(define (average a b) (/ (+ a b) 2.0))

(define (iterative-improve improve close-enough?) 
  (lambda (guess) 
    (let ((next-guess (improve guess))) 
      (if (close-enough? guess next-guess) 
        next-guess 
        ((iterative-improve improve close-enough?) next-guess)))))

(define (sqrt x) 
  ((iterative-improve   
     (lambda (y) (average y (/ x y))) 
     close-enough?) 1.0))

(define (fixed-point f first-guess) 
  ((iterative-improve f close-enough?) first-guess))

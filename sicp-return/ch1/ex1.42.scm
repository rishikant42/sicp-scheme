(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeat f n)
  (if (< n 1)
    (lambda (x) x)
    (compose f (repeat f (- n 1)))))

(define (repeated3 f n)
  (define (iter counter result)
    (if (= counter  1)
      result
      (iter (- counter 1) (compose f result))))
  (iter n f))

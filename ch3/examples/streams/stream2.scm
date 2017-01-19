(define (delay e) (lambda () e))

(define (force fo) (fo))

(define (cons-stream a b)
  (cons a (delay b)))

(define (stream-car s)
  (car s))

(define (stream-cdr s)
  (force (cdr s)))

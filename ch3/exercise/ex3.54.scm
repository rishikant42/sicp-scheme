(define (integer-starting-from n)
  (cons-stream n (integer-starting-from (+ n 1))))

(define integers (integer-starting-from 1))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorial (cons-stream 1 (mul-streams integers factorial)))

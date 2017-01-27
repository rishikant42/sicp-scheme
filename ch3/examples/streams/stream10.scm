(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (partial-sums s) 
  (add-streams s (cons-stream 0 (partial-sums s)))) 

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-streams
  (scale-stream (partial-sums (pi-summands 1)) 4))

(define ones (cons-stream 1 ones))

(define (add-stream s1 s2)
  (stream-map + s1 s2))

(define integers (cons-stream 1 (add-stream ones integers)))

(define fibs (cons-stream 0
                          (cons-stream 1
                                       (add-stream (stream-cdr fibs) fibs))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define double (cons-stream 1 (scale-stream double 2)))

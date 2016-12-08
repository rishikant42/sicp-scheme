(define (make-accumulator sum)
  (lambda (arg)
    (set! sum (+ sum arg))
    sum))

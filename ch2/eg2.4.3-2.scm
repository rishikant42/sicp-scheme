(define (make-from-real-imag x y)
  (define (disptach op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude) (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else (error "Unknown op -- MAKE-FROM-REAL-IMAG"  op))))
  disptach)


(define (apply-generic op arg) (arg op))

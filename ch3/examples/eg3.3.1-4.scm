(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))

  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error "Undefined operation -- CONS" m))))
  dispatch)

(define (car z) (z 'car))

(define (cdr z) (z 'cdr))

(define (set-car! z new-value)
  ((z 'set-car!) new-value)
  z)

(define (set-cdr! z new-value)
  ((z 'set-cdr!) new-value)
  z)

;; TEST

;; 1 ]=> (define z (cons 1 2))
;; 
;; ;Value: z
;; 
;; 1 ]=> (car z)
;; 
;; ;Value: 1
;; 
;; 1 ]=> (cdr z)
;; 
;; ;Value: 2
;; 
;; 1 ]=> (set-car! z 'a)
;; 
;; ;Value 11: #[compound-procedure 11 dispatch]
;; 
;; 1 ]=> (car z)
;; 
;; ;Value: a
;; 
;; 1 ]=> (set-cdr! z 'b)
;; 
;; ;Value 11: #[compound-procedure 11 dispatch]
;; 
;; 1 ]=> (cdr z)
;; 
;; ;Value: b

(define (have-cycle? x)
  (let ((visited '()))
    (define (helper x)
      (cond ((not (pair? x)) #f)
            ((memq x visited) #t)
            (else (set! visited (cons x visited))
                  (or (helper (car x))
                      (helper (cdr x))))))
    (helper x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (last-pair x)
  (if (null? (cdr x))
    x
    (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define c (make-cycle (list 'a 'b 'c)))

;; 1 ]=> (have-cycle? c)
;;
;; ;Value: #t

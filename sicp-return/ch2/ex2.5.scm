;; (define (count-0-rem n divisor)
;;   (define (iter try-exp)
;;     (if (= 0 (remainder n (expt divisor try-exp)))
;;       (iter (+ try-exp 1))
;;       (- try-exp 1)))
;;   (iter 1))
;; 
;; (define (cons a b) (* (expt 2 a) (expt 3 b)))
;; 
;; (define (car z) (count-0-rem z 2))
;; 
;; (define (cdr z) (count-0-rem z 3))

(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Arg -- CONS" m))))
  dispatch)

(define (car z) (z 0))

(define (cdr z) (z 1))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))
 
;; (define (adjoin-set x set)
;;   (define (iter start end)
;;     (cond ((null? end) (append start (list x)))
;;           ((= x (car end)) (append start end))
;;           ((<= x (car end)) (append start (list x) end))
;;           (else (iter (append start (list (car end)))
;;                       (cdr end)))))
;;   (iter '() set))
;;
(define (adjoin-set x set)
  (cond ((null? set) (cons x set))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

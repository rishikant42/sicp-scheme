(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))

(define nil '())

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) nil)
        ((element-of-set? (car set1) set2) (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set2) set1)
        ((null? set1) set2)
        ((not (element-of-set? (car set1) set2)) (cons (car set1) (union-set (cdr set1) set2)))
        (else (union-set (cdr set1) set2))))

;; (define (union-set set1 set2)
;;   (define (iter set result)
;;     (cond ((null? set) result)
;;           ((not (element-of-set? (car set) set2)) (iter (cdr set) (cons (car set) result)))
;;           (else (iter (cdr set) result))))
;;   (iter set1 set2))

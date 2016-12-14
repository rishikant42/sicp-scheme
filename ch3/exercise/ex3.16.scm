(define (count-pairs x)
  (if (not (pair? x))
    0
    (+ (count-pairs (car x))
       (count-pairs (cdr x))
       1)))

;; 1 ]=> (define l1 (list 'a 'b 'c))
;; 
;; ;Value: l1
;; 
;; 1 ]=> (count-pairs l1)
;; 
;; ;Value: 3
;; 
;; 1 ]=> (define l2 (cons l1 '()))
;; 
;; ;Value: l2
;; 
;; 1 ]=> (count-pairs l2)
;; 
;; ;Value: 4
;; 
;; 1 ]=> (define (make-cycle x)
;;         (set-cdr! (last-pair x) x) x)
;; 
;; ;Value: make-cycle
;; 
;; 1 ]=> (define l3 (make-cycle l1))
;; 
;; ;Value: l3
;; 
;; 1 ]=> (count-pairs l3)
;; 
;; ;Aborting!: maximum recursion depth exceeded

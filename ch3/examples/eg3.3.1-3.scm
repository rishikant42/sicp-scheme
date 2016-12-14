(define x (list 'a 'b))

(define z1 (cons x x))

(define z2 (cons (list 'a 'b) (list 'a 'b)))

;; NOTICE HERE

;; 1 ]=> z1
;; 
;; ;Value 11: ((a b) a b)
;; 
;; 1 ]=> z2
;; 
;; ;Value 12: ((a b) a b)
;; 
;; 1 ]=> (eq? z1 z2)
;; 
;; ;Value: #f


;; 1 ]=> (car z1)
;; 
;; ;Value 13: (a b)
;; 
;; 1 ]=> (car z2)
;; 
;; ;Value 14: (a b)
;; 
;; 1 ]=> (eq? (car z1) (car z2))
;; 
;; ;Value: #f


;; 1 ]=> (cdr z1)
;; 
;; ;Value 13: (a b)
;; 
;; 1 ]=> (cdr z2)
;; 
;; ;Value 15: (a b)
;; 
;; 1 ]=> (eq? (cdr z1) (cdr z2))
;; 
;; ;Value: #f

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

;; 1 ]=> z1
;; 
;; ;Value 11: ((a b) a b)
;; 
;; 1 ]=> z2
;; 
;; ;Value 12: ((a b) a b)
;; 
;; 1 ]=> (set-to-wow! z1)
;; 
;; ;Value 11: ((wow b) wow b)
;; 
;; 1 ]=> (set-to-wow! z2)
;; 
;; ;Value 12: ((wow b) a b)

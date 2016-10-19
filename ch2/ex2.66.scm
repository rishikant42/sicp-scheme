(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (key x) (car x))

(define (lookup given-key binary-tree)
  (cond ((null? binary-tree) false)
        ((= given-key (key (car binary-tree))) (car binary-tree))
        ((< given-key (key (car binary-tree))) (lookup given-key (left-branch binary-tree)))
        ((> given-key (key (car binary-tree))) (lookup given-key (right-branch binary-tree)))))


;; 1 ]=> (define l1 (make-tree '(1 a) () ()))
;; 
;; ;Value: l1
;; ;
;; 1 ]=> (define r1 (make-tree '(3 c) () ()))
;; 
;; ;Value: r1
;; 
;; ;1 ]=> (define left (make-tree '(2 b) l1 r1))
;; 
;; Value: left
;; 
;; 1 ]=> (define l2 (make-tree '(5 f) () ()))
;; 
;; ;Value: l2
;; 
;; 1 ]=> (define r2 (make-tree '(7 h) () ()))
;; 
;; ;Value: r2
;; 
;; 1 ]=> (define right (make-tree '(6 g) l2 r2))
;; 
;; ;Value: right
;; 
;; 1 ]=> (define tree (make-tree '(4 d) left right))
;; 
;; ;Value: tree
;; 
;; 1 ]=> tree
;; 
;; ;Value 11: ((4 d) ((2 b) ((1 a) () ()) ((3 c) () ())) ((6 g) ((5 f) () ()) ((7 h) () ())))
;; 
;; 1 ]=> (lookup 5 tree)
;; 
;; ;Value 12: (5 f)
;; 
;; 1 ]=> (lookup 1 tree)
;; 
;; ;Value 13: (1 a)
;; 
;; 1 ]=> (lookup 8 tree)
;; 
;; ;Value: #f

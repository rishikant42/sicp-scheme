(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set)) (element-of-set? x (left-branch set)))
        ((> x (entry set)) (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        
        ((< x (entry set)) (make-tree (entry set)
                                      (adjoin-set x (left-branch set))
                                      (right-branch set)))

        ((> x (entry set)) (make-tree (entry set)
                                      (left-branch set)
                                      (adjoin-set x (right-branch set))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TEST;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1 ]=> (define t1 (make-tree 1 () ()))
;; 
;; ;Value: t1
;; 
;; 1 ]=> (define t2 (make-tree 5 () ()))
;; 
;; Value: t2
;; 
;; 1 ]=> (define left (make-tree 3 t1 t2))
;; 
;; ;Value: left
;; 
;; 1 ]=> left
;; 
;; ;Value 11: (3 (1 () ()) (5 () ()))
;; 
;; 1 ]=> (define t3 (make-tree 8 () () ))
;; 
;; ;Value: t3
;; 
;; 1 ]=> (define right (make-tree 9 t3 () ))
;; 
;; ;Value: right
;; 
;; 1 ]=> right
;; 
;; ;Value 12: (9 (8 () ()) ())
;; 
;; 1 ]=> (define tree (make-tree 7 left right))
;; 
;; ;Value: tree
;; 
;; 1 ]=> tree
;; 
;; ;Value 13: (7 (3 (1 () ()) (5 () ())) (9 (8 () ()) ()))

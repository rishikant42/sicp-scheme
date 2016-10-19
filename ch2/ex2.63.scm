(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (tree->list-1 tree)
  (if (null? tree)
    '()
    (append (tree->list-1 (left-branch tree))
            (cons (entry tree)
                  (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (right-branch tree)
                                        result-list)))))
  (copy-to-list tree '()))

;; Tree (figure 2.16 - 1)
;;
;; 1 ]=> (define l (make-tree 1 () ()))
;; 
;; Value: l
;; 
;; 1 ]=> (define r (make-tree 5 () ()))
;; 
;; ;Value: r
;; 
;; 1 ]=> (define left-tree (make-tree 3 l r))
;; 
;; ;Value: left-tree
;; 
;; 1 ]=> (define r2 (make-tree 11 () ()))
;; 
;; ;Value: r2
;; 
;; 1 ]=> (define right-tree (make-tree 9 () r2))
;; 
;; ;Value: right-tree
;; 
;; 1 ]=> (define tree (make-tree 7 left-tree right-tree))
;;
;; ;Value: tree
;; 
;; 1 ]=> (tree->list-1 tree)
;; 
;; ;Value 11: (1 3 5 7 9 11)
;; 
;; 1 ]=> (tree->list-2 tree)
;; 
;; ;Value 12: (1 3 5 7 9 11)
;;
;;Both procedure will produce same list
;;
;; For tree->list-1:
;; T(n) = 2*T(n/2) + O(n/2) (append take linear time)
;; T(n) = O(n * log n)
;; 
;; For tree->list-2:
;; T(n) = 2*T(n/2) + O(1) (cons take constant time)
;; T(n) = O(n)

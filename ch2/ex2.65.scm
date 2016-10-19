;;;;;;;;;;;;;; EX2.63 ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (tree->list tree)                       ;; only tree->list-2 can be used, since we need O(n) solution
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (right-branch tree)
                                        result-list)))))
  (copy-to-list tree '()))

;;;;;;;;;;;;;;; EX2.64 ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
    (cons '() elts)
    (let ((left-size (quotient (- n 1) 2)))
      (let ((left-result (partial-tree elts left-size)))
        (let ((left-tree (car left-result))
              (non-left-elts (cdr left-result))
              (right-size (- n (+ left-size 1))))
          (let ((this-entry (car non-left-elts))
                (right-result (partial-tree (cdr non-left-elts)
                                            right-size)))
            (let ((right-tree (car right-result))
                  (remaining-elts (cdr right-result)))
              (cons (make-tree this-entry left-tree right-tree)
                    remaining-elts))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Node value of tree can be repeated ;;;;;;;;;;;;;;;

(define (union-set set1 set2)
  (let ((list1 (tree->list set1))
        (list2 (tree->list set2)))
    (list->tree (append list1 list2))))

;; Time ==> tree->list take O(n) time
;;      ==> list->tree take O(n) time
;;      ==> apend take O(n) time
;; hence ==> O(n) + O(n) + O(n)  ===> O(n) time


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Node value of tree cann't be repeated ;;;;;;;;;;;;

(define (element-of-set*? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set*? x (cdr set)))))

(define (union-set* set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((not (element-of-set*? (car set1) set2)) (cons (car set1) (union-set* (cdr set1) set2)))
        (else (union-set* (cdr set1) set2))))

(define (union-set-new set1 set2)
  (let ((list1 (tree->list set1))
        (list2 (tree->list set2)))
    (list->tree (union-set* list1 list2))))

;; Time ==> union-set* take O(n^2) time
;;      ==> total time will be O(n^2)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (entry set1) set2) (make-tree (entry set1) 
                                                        (intersection-set (left-branch set1) set2)
                                                        (intersection-set (right-branch set1) set2)))
        (else (union-set (intersection-set (left-branch set1) set2)
                             (intersection-set (right-branch set1) set2)))))


;; Above soln will not always genrate balance binary tree of intestion

;;;;;;;;;;;;;;;; Balanced intersection tree ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (balanced-intersection set1 set2)
  (let ((intersection-tree (intersection-set set1 set2)))
    (let ((gen-list (tree->list intersection-tree)))
      (list->tree gen-list))))

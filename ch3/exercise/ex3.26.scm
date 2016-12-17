(define (make-table)

  (define (make-record key value) (cons key value))

  (define (record-key record) (car record))

  (define (record-value record) (cdr record))

  (define (entry tree) (car tree))

  (define (left-branch tree) (cadr tree))

  (define (right-brach tree) (caddr tree))

  (define (make-tree entry left right)
    (list entry left right))

  (define (adjoin-set x set)
    (cond ((null? set) (make-tree x '() '()))
          ((= (record-key x) (record-key (entry set)))
           set)
          ((< (record-key x) (record-key (entry set)))
           (make-tree (entry set)
                      (adjoin-set x (left-branch set))
                      (right-brach set)))
          (else
            (make-tree (entry set)
                       (left-branch set)
                       (adjoin-set x (right-brach set))))))

  (let ((local-table '()))

    (define (print) local-table)

    (define (lookup key)
      (define (recur key tree)
        (if (null? tree) 
          false
          (let ((current-key (record-key (entry tree))))
            (cond ((= key current-key)
                   (entry tree))
                  ((< key current-key)
                   (recur key (left-branch tree)))
                  (else
                    (recur key (right-brach tree)))))))
      (recur key local-table))

    (define (insert! key value)
      (set! local-table 
        (adjoin-set (make-record key value)
                    local-table))
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'print-proc) print)
            (else (error "Unkown operation -- TABLE" m))))
    dispatch))

;; (define (lookup key table)
;;     ((table 'lookup-proc) key))
;; 
;; (define (insert! key value table)
;;     ((table 'insert-proc!) key value))

(define operation-table (make-table))

(define get (operation-table 'lookup-proc))

(define put (operation-table 'insert-proc!))

(define print (operation-table 'print-proc))

;; TEST
;; 
;; 1 ]=> (put '1 'apple)
;; 
;; ;Value: ok
;; 
;; 1 ]=> (put '2 'mango)
;; 
;; ;Value: ok
;; 
;; 1 ]=> (get '1)
;; 
;; ;Value 11: (1 . apple)
;; 
;; 1 ]=> (get '2)
;; 
;; ;Value 12: (2 . mango)
;; 
;; 1 ]=> (get '3)
;; 
;; ;Value: #f

(define (make-table)
  (define (assoc key records)
    (cond ((null? records) #f)
          ((equal? key (caar records)) (car records))
          (else (assoc key (cdr records)))))

  (let ((local-table (list '*table*)))

    (define (lookup keys-list)
      (let ((record (assoc keys-list (cdr local-table))))
        (if record
          (cdr record)
          false)))

    (define (insert! keys-list value)
      (let ((record (assoc keys-list (cdr local-table))))
        (if record
          (set-cdr! record value)
          (set-cdr! local-table (cons (cons keys-list value) (cdr local-table)))))
      'ok)

    (define (print) (display local-table))

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'print-proc) print)
            (else (error "Unknow request -- TABLE" m))))

    dispatch))

(define operation-table (make-table))

(define get (operation-table 'lookup-proc))

(define put (operation-table 'insert-proc!))

(define print (operation-table 'print-proc))

;; TEST
;; 
;; 1 ]=> (put 'a 1)
;; 
;; ;Value: ok
;; 
;; 1 ]=> (put '(a b) 2)
;; 
;; ;Value: ok
;; 
;; 1 ]=> (put '(a b c) 3)
;; 
;; ;Value: ok
;; 
;; 1 ]=> (get '(a b c))
;; 
;; ;Value: 3
;; 
;; 1 ]=> (get '(a b))
;; 
;; ;Value: 2
;; 
;; 1 ]=> (get '(a))
;; 
;; ;Value: #f
;; 
;; 1 ]=> (get 'a)
;; 
;; ;Value: 1
;; 
;; 1 ]=> (get '(a d))
;; 
;; ;Value: #f

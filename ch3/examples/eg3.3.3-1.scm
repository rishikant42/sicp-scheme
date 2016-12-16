(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
      (cdr record)
      false)))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
      (set-cdr! record value)
      (set-cdr! table (cons (cons key value) (cdr table))))))

(define (make-table)
  (list '*table*))

;; TEST
;; 
;; 1 ]=> (define t (make-table))
;; 
;; ;Value: t
;; 
;; 1 ]=> t
;; 
;; ;Value 11: (*table*)
;; 
;; 1 ]=> (insert! 'a 1 t)
;; 
;; ;Unspecified return value
;; 
;; 1 ]=> t
;; 
;; ;Value 11: (*table* (a . 1))
;; 
;; 1 ]=> (insert! 'b 2 t)
;; 
;; ;Unspecified return value
;; 
;; 1 ]=> t
;; 
;; ;Value 11: (*table* (b . 2) (a . 1))
;; 
;; 1 ]=> (lookup 'b t)
;; 
;; ;Value: 2
;; 
;; 1 ]=> (lookup 'd t)
;; 
;; ;Value: #f
;; 
;; 1 ]=> (insert! 'c 3 t)
;; 
;; ;Unspecified return value
;; 
;; 1 ]=> t
;; 
;; ;Value 11: (*table* (c . 3) (b . 2) (a . 1))
;; 
;; 1 ]=> (insert! 'b 5 t)
;; 
;; ;Unspecified return value
;; 
;; 1 ]=> t
;; 
;; ;Value 11: (*table* (c . 3) (b . 5) (a . 1))
;; 
;; 1 ]=> (lookup 'b t)
;; 
;; ;Value: 5

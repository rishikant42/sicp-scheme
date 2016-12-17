(define (make-table same-key?)
  (define (assoc key records)
    (cond ((null? records) #f)
          ((same-key? key (caar records)) (car records))
          (else (assoc key (cdr records)))))

  (let ((local-table (list '*table*)))

    (define (lookup key1 key2)
      (let ((subtable (assoc key1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key2 (cdr subtable))))
            (if record
              (cdr record)
              #f))
          #f)))

    (define (insert! key1 key2 value)
      (let ((subtable (assoc key1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key2 (cdr subtable))))
            (if record
              (set-cdr! record value)
              (set-cdr! subtable (cons (cons key2 value) (cdr subtable)))))
          (set-cdr! local-table (cons (list key1 (cons key2 value))
                                      (cdr local-table)))))
      'ok)

    (define (print) local-table)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'print-proc) print)
            (else (error "Unknow request -- TABLE" m))))

    dispatch))


;; (define (insert! key1 key2 value table)
;;   ((table 'insert-proc!) key1 key2 value))
;; 
;; (define (lookup key1 key2 table)
;;   ((table 'lookup-proc) key1 key2))


(define operation-table (make-table equal?))

(define get (operation-table 'lookup-proc))

(define put (operation-table 'insert-proc!))

(define print (operation-table 'print-proc))


;; TEST
;; 
;; 1 ]=> (put 'math '+ 100)
;; 
;; ;Value: ok
;; 
;; 1 ]=> (put 'math '- 200)
;; 
;; ;Value: ok
;; 
;; 1 ]=> (print)
;; 
;; ;Value 11: (*table* (math (- . 200) (+ . 100)))
;; 
;; 1 ]=> (put 'letter 'a 1)
;; 
;; ;Value: ok
;; 
;; 1 ]=> (print)
;; 
;; ;Value 11: (*table* (letter (a . 1)) (math (- . 200) (+ . 100)))
;; 
;; 1 ]=> (put 'letter 'a 11)
;; 
;; ;Value: ok
;; 
;; 1 ]=> (print)
;; 
;; ;Value 11: (*table* (letter (a . 11)) (math (- . 200) (+ . 100)))

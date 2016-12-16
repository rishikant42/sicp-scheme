(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (lookup key1 key2 table)
  (let ((subtable (assoc key1 (cdr table))))
    (if subtable
      (let ((record (assoc key2 (cdr subtable))))
        (if record
          (cdr record)
          #f))
      #f)))

(define (insert! key1 key2 value table)
  (let ((subtable (assoc key1 (cdr table))))
    (if subtable
      (let ((record (assoc key2 (cdr subtable))))
        (if record
          (set-cdr! record value)
          (set-cdr! subtable (cons (cons key2 value) (cdr subtable)))))
      (set-cdr! table (cons (list key1 (cons key2 value))
                            (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

;; TEST
;; 
;; 1 ]=> (define t (make-table))
;; 
;; ;Value: t
;; 
;; 1 ]=> (insert! 'letter 'a 1 t)
;; 
;; ;Value: ok
;; 
;; 1 ]=> t
;; 
;; ;Value 11: (*table* (letter (a . 1)))
;; 
;; 1 ]=> (insert! 'letter 'b 2 t)
;; 
;; ;Value: ok
;; 
;; 1 ]=> t
;; 
;; ;Value 11: (*table* (letter (b . 2) (a . 1)))
;; 
;; 1 ]=> (insert! 'math '+ 100 t)
;; 
;; ;Value: ok
;; 
;; 1 ]=> t
;; 
;; ;Value 11: (*table* (math (+ . 100)) (letter (b . 2) (a . 1)))
;; 
;; 1 ]=> (insert! 'letter 'c 3 t)
;; 
;; ;Value: ok
;; 
;; 1 ]=> t
;; 
;; ;Value 11: (*table* (math (+ . 100)) (letter (c . 3) (b . 2) (a . 1)))
;; 
;; 1 ]=> (insert! 'math '- 200 t)
;; 
;; ;Value: ok
;; 
;; 1 ]=> t
;; 
;; ;Value 11: (*table* (math (- . 200) (+ . 100)) (letter (c . 3) (b . 2) (a . 1)))
;; 
;; 1 ]=> (lookup 'letter 'b t)
;; 
;; ;Value: 2
;; 
;; 1 ]=> (lookup 'letter 'd t)
;; 
;; ;Value: #f
;; 
;; 1 ]=> (lookup 'let 'b t)
;; 
;; ;Value: #f

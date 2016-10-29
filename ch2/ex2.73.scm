(define (install-sum-package) 
  (define (addend s) (car s)) 
  (define (augend s) (cadr s)) 
  (define (make-sum a b) 
    (cond ((eq? a 0) b) 
          ((eq? b 0) a) 
          ((and (number? a) (number? b)) (+ a b)) 
          (else (list '+ a b))))

  (define (deriv-sum s v) 
    (make-sum (deriv (addend s) v) (deriv (augend s) v)))

  (put 'deriv '+ deriv-sum) 
  'done)

(define (install-product-package) 
  (define (multiplier s) (car s)) 
  (define (multiplicand s) (cadr s)) 
  (define (make-product a b) 
    (cond 
      ((or (eq? a 0) (eq? b 0)) 0) 
      ((eq? a 1) b) 
      ((eq? b 1) a) 
      ((and (number? a) (number? b)) (* a b)) 
      (else (list '* a b))) ) 

  (define (make-sum a b) 
    (cond ((eq? a 0) b) 
          ((eq? b 0) a) 
          ((and (number? a) (number? b)) (+ a b)) 
          (else (list '+ a b))))

  (define (deriv-product s v) 
    (make-sum 
      (make-product (deriv (multiplier s) v) (multiplicand s)) 
      (make-product (multiplier s) (deriv (multiplicand s) v)))) 

  (put 'deriv '* deriv-product) 
  'done) 


(define (variable? x) (symbol? x)) 

(define (same-variable? x y) (and (variable? x) (variable? y) (eq? x y))) 

(define (apply-generic op . args) 
  (let ((type-tags (map type-tag args))) 
    (let ((proc (get op type-tags))) 
      (if proc 
        (apply proc (map contents args)) 
        (error "No method for these types -- APPLY-GENERIC" 
               (list op type-tags))))))

(define (deriv exp var) 
  (cond ((number? exp) 0) 
        ((variable? exp) (if (same-variable? exp var) 1 0)) 
        (else ((get 'deriv (operator exp)) (operands exp) var)))) 


(define (operator exp) (car exp)) 
(define (operands exp) (cdr exp)) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; form section 3.3.3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record
              (cdr record)
              false))
          false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
          (set-cdr! local-table
                    (cons (list key-1
                                (cons key-2 value))
                          (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation - TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

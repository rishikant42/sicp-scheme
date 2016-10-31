(define (install-sum-package)
  ;; Internal procedure
  
  (define (addend s) (car s))

  (define (augend s) (cadr s))

  (define (make-sum a1 a2)
    (cond ((eq? a1 0) a2)
          ((eq? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))

  (define (sum-deriv exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))

  ;; Interface to the rest of the system

  (put 'deriv '+ sum-deriv)

  'done)

(define (install-product-package)
  ;; Internal procedure

  (define (multiplier p) (car p))

  (define (multiplicand p) (cadr p))

  (define (make-sum a1 a2)
    (cond ((eq? a1 0) a2)
          ((eq? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))

  (define (make-product m1 m2)
    (cond ((or (eq? m1 0) (eq? m2 0)) 0)
          ((eq? m1 1) m2)
          ((eq? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))

  (define (product-deriv exp var)
    (make-sum (make-product (multiplier exp) (deriv (multiplicand exp) var))
              (make-product (multiplicand exp) (deriv (multiplier exp) var))))

  ;; Interface to the rest of the system

  (put 'deriv '* product-deriv)

  'done)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define (attach-tag type-tag contents)
;;   (cons type-tag contents))
;; 
;; (define (type-tag datum)
;;   (if (pair? datum)
;;     (car datum)
;;     (error "Bad tagged datum -- TYPE-TAG" datum)))
;; 
;; (define (contents datum)
;;   (if (pair? datum)
;;     (cdr datum)
;;     (error "Bad tagged datum -- CONTENTS" datum)))
;; 
;; (define (apply-generic op . args)
;;   (let ((type-tags (map type-tag args)))
;;     (let ((proc (get op type-tags)))
;;       (if proc
;;         (apply proc (map contents args))
;;         (error "No method for these types -- APPLY-GENERIC" (list op type-tags))))))
;; 
;;;;;;;;;;;;;;;;

(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))

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

;; from ex2.78

(define (attach-tag type-tag contents) 
  (if (number? contents) 
    contents 
    (cons type-tag contents))) 

(define (type-tag datum) 
  (cond ((number? datum) 'scheme-number) 
        ((pair? datum) (car datum)) 
        (else (error "Wrong datum -- TYPE-TAG" datum)))) 

(define (contents datum) 
  (cond ((number? datum) datum) 
        ((pair? datum) (cdr datum)) 
        (else (error "Wrong datum -- CONTENTS" datum)))) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error "No method for these types -- APPLY-GENERIC" (list op type-tags))))))

(define (add x y) (apply-generic 'add x y))

(define (sub x y) (apply-generic 'sub x y))

(define (mul x y) (apply-generic 'mul x y))

(define (div x y) (apply-generic 'div x y))

(define (equ x y) (apply-generic 'equ x y))

(define (=zero? arg) (apply-generic 'zero? arg))


;;;;;;;;;;;;;;;;;;;; ORDINARY ARITHMETIC PACKAGE ;;;;;;;;;;;;;;;;;;;

(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))

  (put 'add '(scheme-number scheme-number) (lambda (x y) (tag (+ x y))))

  (put 'sub '(scheme-number scheme-number) (lambda (x y) (tag (- x y))))

  (put 'mul '(scheme-number scheme-number) (lambda (x y) (tag (* x y))))

  (put 'div '(scheme-number scheme-number) (lambda (x y) (tag (/ x y))))

  (put 'make 'scheme-number (lambda (x) (tag x)))

  (put 'zero? '(scheme-number) (lambda (arg) (= arg 0)))

  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; sparse polynomial package

(define (install-sparse-polynomial-package)
  ;; internal procedures
  (define (make-poly variable term-list)
    (cons variable term-list))

  (define (variable p) (car p))

  (define (term-list p) (cdr p))

  (define (variable? x)
    (symbol? x))

  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
      term-list
      (cons term term-list)))

  (define (the-empty-termlist) '())

  (define (first-term term-list) (car term-list))

  (define (rest-terms term-list) (cdr term-list))

  (define (empty-termlist? term-list) (null? term-list))

  (define (make-term order coeff) (list order coeff))

  (define (order term) (car term))

  (define (coeff term) (cadr term))

  ;; add negation procedure for ex2.88
  (define (negation pair) 
    (list (car pair) (- (cadr pair))))

(define (add-terms L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
          (let ((t1 (first-term L1)) (t2 (first-term L2)))
            (cond ((> (order t1) (order t2))
                   (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))

                  ((< (order t1) (order t2))
                   (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                  (else
                    (adjoin-term
                      (make-term (order t1)
                                 (add (coeff t1) (coeff t2)))
                      (add-terms (rest-terms L1)
                                 (rest-terms L2)))))))))

(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
    (the-empty-termlist)
    (add-terms (mul-term-by-all-terms (first-term L1) L2)
               (mul-terms (rest-terms L1) L2))))

(define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
    (the-empty-termlist)
    (let ((t2 (first-term L)))
      (adjoin-term
        (make-term (+ (order t1) (order t2))
                   (mul (coeff t1) (coeff t2)))
        (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1) (term-list p2)))
      (error "Poly not in same var --  ADD-POLY" (list p1 p2))))

  ;; soln of ex2.88
  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1) 
                            (map negation (term-list p2))))
      (error "Poly not in same var --  SUB-POLY" (list p1 p2))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1) (term-list p2)))
      (error "Poly not in same var -- MUL-POLY" (list p1 p2))))

  (define (tag p) (attach-tag 'sparse-polynomial p))

  (put 'add '(sparse-polynomial sparse-polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))

  (put 'mul '(sparse-polynomial sparse-polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))

  (put 'sub '(sparse-polynomial sparse-polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))

  (put 'make 'sparse-polynomial
       (lambda (var terms) (tag (make-poly var terms))))

  'done)

(define (make-sparse-polynomial var terms)
  ((get 'make 'sparse-polynomial) var terms))


;; dense polynomial package

(define (install-dense-polynomial-package)
  ;; internal procedures
  (define (make-poly variable term-list)
    (cons variable term-list))

  (define (variable p) (car p))

  (define (term-list p) (cdr p))

  (define (variable? x) (symbol? x))

  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
      (term-list)
      (let ((exponet (order term))
            (list-exponent (- (length term-list) 1)))
        (cond ((= exponet list-exponent) (cons (+ (coeff term) (car term-list))
                                               (cdr term-list)))
              ((< exponet list-exponent) (cons (car term-list)
                                               (adjoin-term term (cdr term-list))))
              ((> exponet list-exponent) (adjoin-term term (cons 0 term-list)))))))

  (define (the-empty-termlist) '())

  (define (first-term term-list)
    (make-term (- (length term-list) 1)
               (car term-list)))

  (define (rest-terms term-list) (cdr term-list))

  (define (empty-termlist? term-list) (null? term-list))

  (define (make-term order coeff)
    (list order coeff))

  (define (order term) (car term))

  (define (coeff term) (cadr term))

  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else (let ((t1 (first-term L1))
                      (t2 (first-term L2)))
                  (cond ((> (order t1) (order t2)) (adjoin-term t1 (add-terms (rest-terms L1) L2)))
                        ((< (order t1) (order t2)) (adjoin-term t2 (add-terms L1 (rest-terms L2))))
                        (else (adjoin-term (make-term (order t1)
                                                      (add (coeff t1) (coeff t2)))
                                           (add-terms (rest-terms L1)
                                                      (rest-terms L2)))))))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term (make-term (+ (order t1) (order t2))
                                (mul (coeff t1) (coeff t2)))
                     (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY" (list p1 p2))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- MUL-POLY" (list p1 p2))))
  
  ;; interface to rest of the system
  (define (tag p)  (attach-tag 'dense-polynomial p))

  (put 'add '(dense-polynomial dense-polynomial) (lambda (p1 p2) (tag (add-poly p1 p2))))

  (put 'mul '(dense-polynomial dense-polynomial) (lambda (p1 p2) (tag (mul-poly p1 p2))))

  (put 'make 'dense-polynomial (lambda (var terms) (tag (make-poly var terms))))

  'done)

(define (make-dense-polynomial var terms)
  ((get 'make 'dense-polynomial) var terms))

;; TEST

(install-scheme-number-package)
(install-sparse-polynomial-package)
(install-dense-polynomial-package)


;; n = x^3 + 2x^2 + 4x + 1
(define n (make-sparse-polynomial 'x '((3 1 )(2 2) (1 4) (0 1))))

;; m = x^2 + 2x + 5
(define m (make-sparse-polynomial 'x '((2 1) (1 2) (0 5))))


(define a (make-dense-polynomial 'x '(1 2 4 1)))
(define b (make-dense-polynomial 'x '(1 2 5)))

;; 1 ]=> (add n m)
;; 
;; ;Value 11: (sparse-polynomial x (3 1) (2 3) (1 6) (0 6))
;; 
;; 1 ]=> (mul n m)
;; 
;; ;Value 12: (sparse-polynomial x (5 1) (4 4) (3 13) (2 19) (1 22) (0 5))
;; 
;; 1 ]=> (add a b)
;; 
;; ;Value 13: (dense-polynomial x 1 3 6 6)
;; 
;; 1 ]=> (mul a b)
;; 
;; ;Value 14: (dense-polynomial x 1 4 13 19 22 5)

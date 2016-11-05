;; solution of this problem is similar to ex2.93

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


;;;;;;;;;;;;;;;;;;;; RATIONAL ARITHMETIC PACKAGE ;;;;;;;;;;;;;;;;;;;;;;;;;

(define (install-poly-rational-package)
  ;; Internal procedure
  (define (numer x) (car x))

  (define (denom x) (cadr x))

  (define (make-rat n d) (list n d))

  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y)) 
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))

  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))

  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))

  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))

  ;; Interface to the rest of the system
  (define (tag x) (attach-tag 'rational x))

  (put 'add '(rational rational) (lambda (x y) (tag (add-rat x y))))

  (put 'sub '(rational rational) (lambda (x y) (tag (sub-rat x y))))

  (put 'mul '(rational rational) (lambda (x y) (tag (mul-rat x y))))

  (put 'div '(rational rational) (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational (lambda (n d) (tag (make-rat n d))))

  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))


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



(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
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

  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
          (list (the-empty-termlist) L1)
          (let ((new-c (div (coeff t1) (coeff t2)))
                (new-o (- (order t1) (order t2))))
            (let ((rest-of-result (div-terms 
                                    (add-terms L1 
                                               (map negation (mul-term-by-all-terms             ;; (L1 - (quotient * L2))
                                                               (make-term new-o new-c) L2)))
                                    L2)))
              (cons (adjoin-term (make-term new-o new-c)
                                 (car rest-of-result))
                    (cdr rest-of-result))))))))

  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (div-terms (term-list p1) (term-list p2)))
      (error "Poly not in same var -- DIV-POLY" (list p1 p2))))

   (define (remainder-terms p1 p2) 
     (cadr (div-terms p1 p2))) 

   (define (gcd-terms a b) 
     (if (empty-termlist? b) 
       a 
       (gcd-terms b (remainder-terms a b)))) 

   (define (gcd-poly p1 p2) 
     (if (same-variable? (variable p1) (variable p2)) 
       (make-poly (variable p1) 
                  (gcd-terms (term-list p1) 
                             (term-list p2))) 
       (error "not the same variable -- GCD-POLY" (list p1 p2))))

   (define (tag p) (attach-tag 'polynomial p))

   (put 'add '(polynomial polynomial)
        (lambda (p1 p2) (tag (add-poly p1 p2))))

   (put 'mul '(polynomial polynomial)
        (lambda (p1 p2) (tag (mul-poly p1 p2))))

   (put 'sub '(polynomial polynomial)
        (lambda (p1 p2) (tag (sub-poly p1 p2))))

   (put 'make 'polynomial
        (lambda (var terms) (tag (make-poly var terms))))

   (put 'div '(polynomial polynomial)
        (lambda (p1 p2) (tag (div-poly p1 p2))))

   (put 'gcd-poly '(polynomial polynomial) (lambda (p1 p2) (gcd-poly (cdr p1) (cdr p2))))


   'done)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(define (greatest-common-divisor p1 p2)
  ((get 'gcd-poly '(polynomial polynomial)) p1 p2)) 

;; TEST

(install-scheme-number-package)
(install-polynomial-package)
(install-poly-rational-package)

;; 1 ]=> (define p1 (make-polynomial 'x '((4 1) (3 -1) (2 -2) (1 2))))
;; 
;; ;Value: p1
;; 
;; 1 ]=> (define p2 (make-polynomial 'x '((3 1) (1 -1))))
;; 
;; ;Value: p2

;; 1 ]=> (greatest-common-divisor p1 p2)

;; ;Value 11: (x (2 -1) (1 1))

;; 1 ]=> (define gcd (make-polynomial 'x '((2 -1) (1 1))))
;; 
;; ;Value: gcd
;; 
;; 1 ]=> (div p1 gcd)
;; 
;; ;Value 12: (polynomial x ((2 -1) (0 2)) ())                  quotient: -x^2 + 2          rem: 0
;; 
;; 1 ]=> (div p2 gcd)
;; 
;; ;Value 13: (polynomial x ((1 -1) (0 -1)) ())                 quotient: -x - 1            rem: 0

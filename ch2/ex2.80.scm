(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
    (cdr datum)
    (error "Bad tagged datum -- CONTENTS" datum)))

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

  (put 'equ '(scheme-number scheme-number) (lambda (x y) (= x y)))

  (put 'zero? '(scheme-number) (lambda (arg) (= arg 0)))

  (put 'make 'scheme-number (lambda (x) (tag x)))

  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

;;;;;;;;;;;;;;;;;;;; RATIONAL ARITHMETIC PACKAGE ;;;;;;;;;;;;;;;;;;;;;;;;;

(define (install-rational-package)
  ;; Internal procedure
  (define (numer x) (car x))

  (define (denom x) (cdr x))

  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))

  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y)) 
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))

  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))

  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))

  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

  (define (equal-rat? x y)
    (= (* (numer x) (denom y))
       (* (numer y) (denom x))))

  (define (=zero-rat? r)
    (= (numer r) 0))

  ;; Interface to the rest of the system
  (define (tag x) (attach-tag 'rational x))

  (put 'add '(rational rational) (lambda (x y) (tag (add-rat x y))))

  (put 'sub '(rational rational) (lambda (x y) (tag (sub-rat x y))))

  (put 'mul '(rational rational) (lambda (x y) (tag (mul-rat x y))))

  (put 'div '(rational rational) (lambda (x y) (tag (div-rat x y))))

  (put 'equ '(rational rational) (lambda (x y) (equal-rat? x y)))

  (put 'zero? '(rational) (lambda (r) (=zero-rat? r)))

  (put 'make 'rational (lambda (n d) (tag (make-rat n d))))

  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FROM SECTION 2.4.3 ;;;;;;;;;;;;;;;;;;;;;;;

(define (install-rectangular-package)
  ;; Internal procedure

  (define (make-from-real-imag x y) (cons x y))

  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  (define (real-part z) (car z))

  (define (imag-part z) (cdr z))

  (define (magnitude z)
    (sqrt (+ (square (real-part z)) (square (imag-part z)))))

  (define (angle z)
    (atan (imag-part z) (real-part z)))

  ;; Interface to the rest of system

  (define (tag x) (attach-tag 'rectangular x))

  (put 'real-part '(rectangular) real-part)

  (put 'imag-part '(rectangular) imag-part)

  (put 'magnitude '(rectangular) magnitude)

  (put 'angle '(rectangular) angle)

  (put 'make-from-real-imag 'rectangular (lambda (x y) (tag (make-from-real-imag x y))))

  (put 'make-from-mag-ang 'rectangular (lambda (r a) (tag (make-from-mag-ang r a))))

  'done)

(define (install-polar-package)
  ;; Internal procedure

  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y))) (atan y x)))

  (define (make-from-mag-ang r a) (cons r a))

  (define (real-part z) (* (magnitude z) (cos (angle z))))

  (define (imag-part z) (* (magnitude z) (sin (angle z))))

  (define (magnitude z) (car z))

  (define (angle z) (cdr z))

  ;; Interface to the rest of system

  (define (tag x) (attach-tag 'polar x))

  (put 'real-part '(polar) real-part)

  (put 'imag-part '(polar) imag-part)

  (put 'magnitude '(polar) magnitude)

  (put 'angle '(polar) angle)

  (put 'make-from-real-imag 'polar (lambda (x y) (tag (make-from-real-imag x y))))

  (put 'make-from-mag-ang 'polar (lambda (r a) (tag (make-from-mag-ang r a))))

  'done)


(define (real-part z) (apply-generic 'real-part z))

(define (imag-part z) (apply-generic 'imag-part z))

(define (magnitude z) (apply-generic 'magnitude z))

(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; COMPLEX ARITHMETIC PACKAGE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (install-complex-package)
  ;; Imported procedure from rectangular and polar package
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))

  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  ;; Internal procedure
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))

  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))

  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))

  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

  (define (eql-complex? z1 z2)
    (and (= (real-part z1) (real-part z2))
         (= (imag-part z1) (imag-part z2))))

  (define (=zero-complex? c)
    (and (= (real-part c) 0) (= (imag-part c) 0)))

  ;; Interface to rest of the system
  (define (tag x) (attach-tag 'complex x))

  (put 'add '(complex complex) (lambda (z1 z2) (tag (add-complex z1 z2))))

  (put 'sub '(complex complex) (lambda (z1 z2) (tag (sub-complex z1 z2))))

  (put 'mul '(complex complex) (lambda (z1 z2) (tag (mul-complex z1 z2))))

  (put 'div '(complex complex) (lambda (z1 z2) (tag (div-complex z1 z2))))

  (put 'equ '(complex complex) (lambda (z1 z2) (eql-complex? z1 z2)))

  (put 'zero? '(complex) (lambda (c) (=zero-complex? c)))

  (put 'make-from-real-imag 'complex (lambda (x y) (tag (make-from-real-imag x y))))

  (put 'make-from-mag-ang 'complex (lambda (r a) (tag (make-from-mag-ang r a))))

  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

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

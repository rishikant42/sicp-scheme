(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
    (begin (set! balance (- balance amount))
           balance)
    "Insufficients funds"))

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))))

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds")))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

  (define (deposite amount)
    (set! balance (+ balance amount))
    balance)

  (define (dispatch m)
    (cond ((eq? 'withdraw m) withdraw)
          ((eq? 'deposite m) deposite)
          (else (error "unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

(define (test x)
  (cond ((= x 1) (display (+ x 5)) (display (+ x 10)))
        ((= x 2) (display (* x 5)) (display (* x 10)))
        ((= x 3) (display (- x 5)) (display (- x 10)))))


(define (append x y)
  (if (null? x)
    y
    (cons (car x) (append (cdr x) y))))

;; (append '(a b c) '(d e f))
;; (cons 'a (append '(b c) '(d e f)))
;; (cons 'a (cons 'b (append '(c) '(d e f))))
;; (cons 'a (cons 'b (cons 'c (append '() '(d e f)))))
;; (cons 'a (cons 'b (cons 'c '(d e f))))
;; (a b c d e f)

(define (last-pair x)
  (if (null? (cdr x))
    x
    (last-pair (cdr x))))

;; (last-pair '(a b c))
;; (last-pair '(b c))
;; (last-pair '(c))
;; (c)

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

;; (append! '(a b c) '(d e f))
;; (set-cdr! (last-pair '(a b c)) '(d e f))
;; (set-cdr! '(c) '(d e f))
;; (a b c d e f)


(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1 ]=> (define r (list (cons 'a 1) (cons 'b 2) (cons 'c 3)))
;; 
;; ;Value: r
;; 
;; 1 ]=> r
;; 
;; ;Value 11: ((a . 1) (b . 2) (c . 3))
;; 
;; 1 ]=> (assoc 'a r)
;; 
;; ;Value 12: (a . 1)
;; ;
;; 1 ]=> (assoc 'b r)
;; 
;; ;Value 13: (b . 2)
;; 
;; 1 ]=> (assoc 'c r)
;; 
;; ;Value 14: (c . 3)
;; 
;; 1 ]=> (assoc 'd r)
;; 
;; ;Value: #f

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
      (cdr record)
      #f)))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
      (set-cdr! record value)
      (set-cdr! table (cons (cons key value) (cdr table))))))

(define (make-table)
  (list '*table*))

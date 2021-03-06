(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp) (make-sum (make-product (multiplier exp) (deriv (multiplicand exp) var))
                                  (make-product (multiplicand exp) (deriv (multiplier exp) var))))
        ((exponentiation? exp) (make-product (make-product (exponent exp) 
                                                           (make-exponentiation (base exp) (make-sum (exponent exp) '-1)))
                                             (deriv (base exp) var)))
        (else (error "Unknown expression type -- DERIV" exp))))

(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (make-exponentiation base exponent)
  (cond ((=number? base 1) 1)
        ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (power base exponent))
        (else (list base '** exponent))))

(define (power b n)
  (if (= n 0)
    1
    (* b (power b (- n 1)))))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s)
  (car s))

(define (augend s)
  (caddr s))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p)
  (car p))

(define (multiplicand p)
  (caddr p))

;; (** x y) ==> x^y ==> x to the power y

(define (exponentiation? e)
  (and (pair? e) (eq? (cadr e) '**)))

(define (base e)
  (car e))

(define (exponent e)
  (caddr e))

;; 1 ]=> (deriv '(x ** 5) 'x)
;; 
;; Value 11: (5 * (x ** 4))
;; 
;; 1 ]=> (deriv '((x * y) * (x + 3)) 'x)
;; 
;; ;Value 12: ((x * y) + ((x + 3) * y))

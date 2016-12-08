(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

  (define (deposite amount)
    (set! balance (+ balance amount))
    balance)

  (define error-msg
    (lambda (x) "Incorrect password"))

  (define (dispatch passwd m)
    (cond ((not (eq? passwd password)) error-msg) ;; all other cases of cond rtrn prcdr, so it shld rtrn prcdr
          ((eq? 'withdraw m) withdraw)
          ((eq? 'deposite m) deposite)
          (else (error "Unknow request -- MAKE-ACCOUNT" m))))

  dispatch)

;; 1 ]=> (define acc (make-account 100 'hello))
;; 
;; ;Value: acc
;; 
;; 1 ]=> ((acc 'hello 'withdraw) 30)
;; 
;; ;Value: 70
;; 
;; 1 ]=> ((acc 'hello 'deposite) 40)
;; 
;; ;Value: 110
;; 
;; 1 ]=> ((acc 'hell 'deposite) 40)
;; 
;; ;Value 11: "Incorrect password"

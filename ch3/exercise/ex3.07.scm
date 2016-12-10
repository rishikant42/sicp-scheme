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

  (define (joint newpasswd)
    (let ((new-password newpasswd))
      (define (joint-dispatch passwd m)
        (cond ((not (eq? passwd new-password)) error-msg) 
              ((eq? 'withdraw m) withdraw)
              ((eq? 'deposite m) deposite)
              (else (error "Unknow request -- MAKE-ACCOUNT" m))))
      joint-dispatch))

  (define (dispatch passwd m)
    (cond ((not (eq? passwd password)) error-msg) 
          ((eq? 'withdraw m) withdraw)
          ((eq? 'deposite m) deposite)
          ((eq? 'joint m) joint)
          (else (error "Unknow request -- MAKE-ACCOUNT" m))))

  dispatch)

(define (make-joint account passwd newpasswd)
  ((account passwd 'joint) newpasswd))

;; TESTS

;; 1 ]=> (define acc1 (make-account 100 'hello))
;; 
;; ;Value: acc1
;; 
;; 1 ]=> (define acc2 (make-joint acc1 'hello 'world))
;; 
;; ;Value: acc2
;; ;
;; 1 ]=> ((acc1 'hello 'withdraw) 40) 
;; 
;; ;Value: 60
;; 
;; 1 ]=> ((acc2 'world 'deposite) 70) 
;; 
;; ;Value: 130
;; 
;; 1 ]=> ((acc1 'hello 'deposite) 20) 
;; 
;; ;Value: 150
;; 
;; 1 ]=> ((acc1 'hell 'deposite) 20) 
;; 
;; ;Value 11: "Incorrect password"
;; 
;; 1 ]=> ((acc2 'hello 'deposite) 20) 
;; 
;; ;Value 11: "Incorrect password"

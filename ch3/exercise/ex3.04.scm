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

  (define wrong-passwd-count 0)

  (define call-the-cops
    (lambda (x) "Congrats! Police are coming. Wait for a moment"))

  (define (dispatch passwd m)
    (cond ((not (eq? passwd password)) (set! wrong-passwd-count (+ wrong-passwd-count 1)) 
                                       (if (> wrong-passwd-count 7)
                                         call-the-cops
                                         error-msg))
          ((eq? 'withdraw m) (set! wrong-passwd-count 0) withdraw)
          ((eq? 'deposite m) (set! wrong-passwd-count 0) deposite)
          (else (error "Unknow request -- MAKE-ACCOUNT" m))))

  dispatch)

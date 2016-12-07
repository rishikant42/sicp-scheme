(define new-withdraw 
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))))


;; 1 ]=> (new-withdraw 25)
;; 
;; ;Value: 75
;; 
;; 1 ]=> (new-withdraw 25)
;; 
;; ;Value: 50
;; 
;; 1 ]=> (new-withdraw 60)
;; 
;; ;Value 11: "Insufficient funds"
;; 
;; 1 ]=> (new-withdraw 20)
;; 
;; ;Value: 30

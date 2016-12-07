(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds")))

;; 1 ]=> (define w1 (make-withdraw 100))
;; 
;; ;Value: w1
;; 
;; 1 ]=> (define w2 (make-withdraw 100))
;; 
;; ;Value: w2
;; 
;; 1 ]=> (w1 50)
;; 
;; ;Value: 50
;; 
;; 1 ]=> (w2 70)
;; 
;; ;Value: 30
;; 
;; 1 ]=> (w2 40)
;; 
;; ;Value 11: "Insufficient funds"
;; 
;; 1 ]=> (w1 40)
;; 
;; ;Value: 10

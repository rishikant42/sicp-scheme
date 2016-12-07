(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
    (begin (set! balance (- balance amount))
           balance)
    "Insufficient funds"))

;; 1 ]=> (withdraw 25)
;; 
;; ;Value: 75
;; 
;; 1 ]=> (withdraw 25)
;; 
;; ;Value: 50
;; 
;; 1 ]=> (withdraw 60)
;; 
;; ;Value 11: "Insufficient funds"
;; 
;; 1 ]=> (withdraw 15)
;; 
;; ;Value: 35

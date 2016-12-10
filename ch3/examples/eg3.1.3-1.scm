;; functional programming version

(define (factorial1 n)
  (define (iter product counter)
    (if (> counter n)
      product
      (iter (* counter product) (+ counter 1))))
  (iter 1 1))

;; 1 ]=> (factorial1 5)
;; 
;; ;Value: 120

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; imperative programming version

(define (factorial2 n)
  (let ((product 1)
        (counter 1))
    (define (iter)
      (if (> counter n)
        product
        (begin (set! product (* product counter))
               (Set! counter (+ counter 1))
               (iter))))
    (iter)))
 
;; 1 ]=> (factorial2 5)
;; 
;; ;Value: 120

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Pitfalls of imperative programming
;; order of Assaignments should be proper, otherwise program may return incorrect answer
;; Example

(define (factorial3 n)
  (let ((product 1)
        (counter 1))
    (define (iter)
      (if (> counter n)
        product 
        (begin (set! counter (+ counter 1))
               (set! product (* product counter))
               (iter))))
    (iter)))

;; 1 ]=> (factorial3 5)
;; 
;; ;Value: 720                  
;;
;; Above result is wrong

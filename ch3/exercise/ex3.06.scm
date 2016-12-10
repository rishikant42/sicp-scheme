(define init 10)

(define (rand m)
  (cond ((eq? 'generate m) (random init))
        ((eq? 'reset m) (lambda (new) (set! init new)))
        (else (error "Unknown request -- RAND" m))))

;; TESTS
;; 
;; 1 ]=> (rand 'generate)
;; 
;; ;Value: 2
;; 
;; 1 ]=> (rand 'generate)
;; 
;; ;Value: 5
;; 
;; 1 ]=> ((rand 'reset) 5000)
;; 
;; ;Value: 10
;; 
;; 1 ]=> (rand 'generate)
;; 
;; ;Value: 514
;; 
;; 1 ]=> (rand 'generate)
;; 
;; ;Value: 2043

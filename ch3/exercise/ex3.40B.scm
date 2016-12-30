;; (define x 10)
;; (parallel-execute (lambda () (set! x (* x x)))
;;                   (lambda () (set! x (* x x x))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; p1 : 10 * 10 = 100                          first execute p1, set x to 100
;; p2 : 100 * 100 * 100 = 1000000              then execute p2, set x to 1000000
;; 
;; 
;; p2 : 10 * 10 * 10 = 1000                   first execute p2, set x to 1000
;; p1 : 1000 * 1000  = 1000000                then execute p1, set x to 1000000
;; 
;; 
;; possible value of x = 1000000

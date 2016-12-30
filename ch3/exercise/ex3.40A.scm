;; (define x 10)
;; (parallel-execute (lambda () (set! x (* x x)))
;;                   (lambda () (set! x (* x x x))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; p1  :  10 * 10                              p1 set x to 100
;; p2  :  100 * 100 * 100                      p2 access x as 100 & return 1000000
;; 
;; 
;; p1  :  1000 * 1000                          p1 access x as 1000 & return 1000000
;; p2  :  10 * 10 * 10                         p2 set x to 1000
;; 
;; 
;; p1  :  10 * 1000                            p1 access first x of (* x x) as 10 & second x as 1000 (i.e x set by p2 b/w execution) & return 10000
;; p2  :  10 * 10 * 10                         p2 set x 1000 (p1 execution is still in progress)
;; 
;; 
;; p1  :  10 * 10                              p1 set x to 100 (p2 execution is in running)
;; p2  :  10 * 10 * 100                        p2 access first & second x of (* x x x) as 10 & third as 100  & return 10000
;; 
;; 
;; p1  :  10 * 10
;; p2  :  10 * 100 * 100                      retrun 100000
;; 
;; 
;; p1  :  10 * 10                             both p1 & p2 access x as 10 first p1 set x to 100 then p2 set x to 1000
;; p2  :  10 * 10 * 10                        final x is 1000
;; 
;; 
;; p1  :  10 * 10                             both p1 & p2 access x as 10 , first p2 set x to 1000 then p1 set x to 100
;; p2  :  10 * 10 * 10                        final x is 100
;; 
;; 
;; possible value of x = {100, 1000, 10000, 100000, 1000000}

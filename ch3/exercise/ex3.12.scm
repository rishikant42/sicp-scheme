;; 1 ]=> (define x (list 'a 'b))
;; 
;; ;Value: x
;; 
;; 1 ]=> (define y (list 'c 'd))
;; 
;; ;Value: y
;; 
;; 1 ]=> (define z (append x y))
;; 
;; ;Value: z
;; 
;; 1 ]=> z
;; 
;; ;Value 11: (a b c d)
;; 
;; 1 ]=> (cdr x)
;; 
;; ;Value 12: (b)

;; Ans: append use cons operation , which make new pair whenever it call
;;      append doesn't do anything with existing list x


;;;;;;;;;;;;;;;;;;;;;; RESTART REPL ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; 1 ]=> (define x (list 'a 'b))
;; 
;; ;Value: x
;; 
;; 1 ]=> (define y (list 'c 'd))
;; 
;; ;Value: y
;; 
;; 1 ]=> (define w (append! x y))
;; 
;; ;Value: w
;; 
;; 1 ]=> w
;; 
;; ;Value 11: (a b c d)
;; 
;; 1 ]=> (cdr x)
;; 
;; ;Value 12: (b c d)

;; Ans: append! use set-cdr! operation which modify the existing pair. set-cdr! is used only for their efffect, not for value.
;;      append! modify existing list x

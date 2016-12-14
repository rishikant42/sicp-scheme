;; 1 ]=> (define x '((a b) c d))
;; 
;; ;Value: x
;; 
;; 1 ]=> (define y '(e f))
;; 
;; ;Value: y
;; 
;; 1 ]=> x
;; 
;; ;Value 11: ((a b) c d)
;; 
;; 1 ]=> (set-cdr! x y)
;; 
;; ;Unspecified return value
;; 
;; 1 ]=> x
;; 
;; ;Value 11: ((a b) e f)

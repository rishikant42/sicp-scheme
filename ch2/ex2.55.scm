;; 1 ]=> (define test '(a b c '(d e) '(f '(g h))))
;; 
;; ;Value: test
;; 
;; 1 ]=> test
;; 
;; ;Value 11: (a b c (quote (d e)) (quote (f (quote (g h)))))
;; 
;; 1 ]=> (define test2 ''abracadabra)
;; 
;; ;Value: test2
;; 
;; 1 ]=> test2
;; 
;; ;Value 11: (quote abracadabra)
;; 
;; 1 ]=> (car test2)
;; 
;; ;Value: quote

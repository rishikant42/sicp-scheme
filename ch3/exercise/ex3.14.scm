(define (mystery x)
  (define (loop x y)
    (if (null? x)
      y
      (let ((temp (cdr x)))
        (set-cdr! x y)
        (loop temp x))))
  (loop x '()))

;; 1 ]=> (define v (list 'a 'b 'c 'd))
;; 
;; ;Value: v
;; 
;; 1 ]=> (define w (mystery v))
;; 
;; ;Value: w
;; 
;; 1 ]=> v
;; 
;; ;Value 11: (a)
;; 
;; 1 ]=> w
;; 
;; ;Value 12: (d c b a)

;; (mystery '(a b c d))
;; (loop '(a b c d) '())
;; (loop '(b c d)  '(a))
;; (loop '(c d) '(b a))
;; (loop '(d) '(c b a))
;; (loop '() '(d c b a))

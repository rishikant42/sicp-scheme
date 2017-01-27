(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (partial-sums s) 
  (add-streams s (cons-stream 0 (partial-sums s)))) 

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

;; 1 ]=> (define s (euler-transform pi-stream))
;; 
;; ;Value: s
;; 
;; 1 ]=> s
;; 
;; ;Value 11: (3.166666666666667 . #[promise 12])
;; 
;; 1 ]=> (stream-cdr s)
;; 
;; ;Value 13: (3.1333333333333337 . #[promise 14])
;; 
;; 1 ]=> (stream-cdr (stream-cdr s))
;; 
;; ;Value 15: (3.1452380952380956 . #[promise 16])

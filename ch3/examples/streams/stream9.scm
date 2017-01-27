(define (average a b)
  (/ (+ a b) 2.0))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

;; 1 ]=> (define s (sqrt-stream 2))
;; 
;; ;Value: s
;; 
;; 1 ]=> s
;; 
;; ;Value 14: (1. . #[promise 15])
;; 
;; 1 ]=> (stream-cdr s)
;; 
;; ;Value 16: (1.5 . #[promise 17])
;; 
;; 1 ]=> (stream-cdr (stream-cdr s))
;; 
;; ;Value 18: (1.4166666666666665 . #[promise 19])

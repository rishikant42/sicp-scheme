(define stream-null? null?)

(define the-empty-stream '())

(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
    the-empty-stream
    (cons-stream (proc (stream-car s))
                 (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
    'done
    (begin (proc (stream-car s))
           (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (stream-enumerate-interval low high)
  (if (> low high)
    the-empty-stream
    (cons-stream low
                 (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream)) (cons-stream (stream-car stream)
                                                 (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (stream-list . args)
  (define (helper items)
    (if (null? items)
      the-empty-stream
      (cons-stream (car items)
                   (helper (cdr items)))))
  (helper args))


(define (cons-stream a b)
  (cons a (delay b)))

(define (stream-car s) (car s))

(define (stream-cdr s) (force (cdr s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (average x y) (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

;; (define (sqrt-stream x)
;;   (define guesses
;;     (cons-stream 1.0
;;                  (stream-map (lambda (guess)
;;                                (sqrt-improve guess x))
;;                              guesses)))
;;   guesses)

;; (define (pi-summands n)
;;   (cons-stream (/ 1.0 n)
;;                (stream-map - (pi-summands (+ n 2)))))
;; 
;; (define pi-stream (scale-stream (partial-sums (pi-summands 1)) 4))

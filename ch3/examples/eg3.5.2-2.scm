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

;; (define ones (cons-stream 1 ones))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
    the-empty-stream
    (cons-stream (apply proc (map stream-car argstreams))
                 (apply stream-map (cons proc (map stream-cdr argstreams))))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

;; 1 ]=> (define s1 (stream-list 1 2 3 4 5))
;; 
;; ;Value: s1
;; 
;; 1 ]=> (define s2 (stream-list 6 7 8 9 10))
;; 
;; ;Value: s2
;; 
;; 1 ]=> (define s3 (add-streams s1 s2))
;; 
;; ;Value: s3
;; 
;; 1 ]=> (display-stream s3)
;; 
;; 7
;; 9
;; 11
;; 13
;; 15
;; ;Value: done

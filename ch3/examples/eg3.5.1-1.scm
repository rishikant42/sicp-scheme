(define nil '())

(define (enumerate-interval a b)
  (if (> a b)
    nil
    (cons a (enumerate-interval (+ a 1) b))))

(define (filter pred? items)
  (cond ((null? items) nil)
        ((pred? (car items)) (cons (car items) (filter pred? (cdr items))))
        (else (filter pred? (cdr items)))))

(define (accumulate op init items)
  (if (null? items)
    init
    (op (car items)
        (accumulate op init (cdr items)))))

(define (map proc items)
  (if (null? items)
    nil
    (cons (proc (car items))
          (map proc (cdr items)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n divisor)
  (cond ((> (square divisor) n) n)
        ((devides? n divisor) divisor)
        (else (find-divisor n (+ divisor 1)))))

(define (devides? n divisor)
  (= (remainder n divisor) 0))

(define (prime? n)
  (= (smallest-divisor n) n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sum-primes1 a b)
  (define (iter count accum)
    (cond ((> count b) accum)
          ((prime? count) (iter (+ count 1) (+ accum count)))
          (else (iter (+ count 1) accum))))
  (iter a 0))

(define (sum-primes2 a b)
  (accumulate + 0 (filter prime? (enumerate-interval a b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; return second prime in given range

(define (sec-prime1 a b)
  (define (iter curr counter)
    (cond ((> curr b) (display "no second prime"))
          ((prime? curr) (if (= counter 1) 
                           curr
                           (iter (+ curr 1) (+ counter 1))))
          (else (iter (+ curr 1) counter))))
  (iter a 0))


(define (sec-prime2 a b)
  (car (cdr (filter prime? (enumerate-interval a b)))))

;; 1 ]=> (sec-prime1 1 100)
;; 
;; ;Value: 2
;; 
;; 1 ]=> (sec-prime2 1 100)
;; 
;; ;Value: 2
;; 
;; 1 ]=> (sec-prime1 10000 1000000)
;; 
;; ;Value: 10009
;; 
;; 1 ]=> (sec-prime2 10000 1000000)
;; 
;; ;Aborting!: maximum recursion depth exceeded

;; Hence it is clear, first apporach (i.e sec-prime1 procedure) is much better than second (i.e sec-prime2 procedure)

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

(define (filtered-accumulate combiner null-value term a next b filter)
  (define (iter a result)
    (if (> a b)
      result
      (if (filter a)
        (iter (next a) (combiner result (term a)))
        (iter (next a) (combiner result null-value)))))
  (iter a null-value))

(define (sum term a next b filter)
  (filtered-accumulate + 0 term a next b filter))

(define (identity x) x)
(define (inc n) (+ n 1))

(define (sum-prime a b)
  (sum identity a inc b prime?))

(define (sum-prime-sqr a b)
  (sum square a inc b prime?))

(define (product term a next b filter)
  (filtered-accumulate * 1 term a next b filter))

(define (product-prime a b)
  (product identity a inc b prime?))

(define (product-prime-sqr a b)
  (product square a inc b prime?))




(define (sum-prime2 a b)
  (define (iter a result)
    (cond ((> a b) result)
          ((prime? a) (iter (inc a) (+ result a)))
          (else (iter (inc a) (+ result 0)))))
  (iter a 0))

(define (sum-prime-sqr2 a b)
  (define (iter a result)
    (cond ((> a b) result)
          ((prime? a) (iter (inc a) (+ result (square a))))
          (else (iter (inc a) (+ result 0)))))
  (iter a 0))

(define (product-prime2 a b)
  (define (iter a result)
    (cond ((> a b) result)
          ((prime? a) (iter (inc a) (* result a)))
          (else (iter (inc a) (* result 1)))))
  (iter a 1))

(define (product-prime-sqr2 a b)
  (define (iter a result)
    (cond ((> a b) result)
          ((prime? a) (iter (inc a) (* result (square a))))
          (else (iter (inc a) (* result 1)))))
  (iter a 1))

;; Above procedures will generate iterative process

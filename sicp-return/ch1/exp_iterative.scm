(define (expt-iter b counter product)
  (if (= counter 0)
    product
    (expt-iter b
               (- counter 1)
               (* b product))))


(define (expt b n)
  (expt-iter b n 1))

;; space complexity = O(1)
;; time complexity = O(n)

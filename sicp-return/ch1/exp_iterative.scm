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

;; (expt 2 5)
;;
;; (expt-iter 2 5 1)
;;
;; (expt-iter 2 4 2)
;;
;; (expt-iter 2 3 4)
;;
;; (expt-iter 2 2 8)
;;
;; (expt-iter 2 1 16)
;;
;; (expt-iter 2 0 32)

(define (square x) (* x x))

(define (expt-iter b n result)
  (cond ((= n 0) result)
        ((even? n) (expt-iter (square b) (/ n 2) result))
        (else (expt-iter b (- n 1) (* b result)))))

(define (expt b n)
  (expt-iter b n 1))

;; time complexity = O(log(n))
;; space complexity = O(1)

;; (expt 2 5)
;;
;; (expt-iter 2 5 1)
;;
;; (expt-iter 2 4 2)
;;
;; (expt-iter 4 2 2)
;;
;; (expt-iter 16 1 2)
;;
;; (expt-iter 16 0 32)
;;
;; 32

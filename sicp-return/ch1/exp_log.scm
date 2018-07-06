(define (square x) (* x x))

(define (expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (expt b (/ n 2))))
        (else (* b (expt b (- n 1))))))

;; space complexity = O(log(n))
;; time complexity = O(log(n))

;; (expt 2 5)
;;
;; (* 2 (expt 2 4))
;;
;; (* 2 (square (expt 2 2)))
;;
;; (* 2 (square (square (expt 2 1))))
;;
;; (* 2 (square (square (* 2 (expt 2 0)))))
;;
;; (* 2 (square (square (* 2 1))))
;;
;; (* 2 (square (square 2)))
;;
;; (* 2 (square 4))
;;
;; (* 2 16)
;;
;; 32

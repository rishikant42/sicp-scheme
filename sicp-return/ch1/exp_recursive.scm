(define (expt b n)
  (if (= n 0)
    1
    (* b (expt b (- n 1)))))

;; space complexity = O(n)
;; time complexity = O(n)

;; (expt 2 5)
;;
;; (* 2 (expt 2 4))
;;
;; (* 2 (* 2 (expt 2 3)))
;;
;; (* 2 (* 2 (* 2 (expt 2 2))))
;;
;; (* 2 (* 2 (* 2 (* 2 (expt 2 1)))))
;;
;; (* 2 (* 2 (* 2 (* 2 (* 2 (expt 2 0))))))
;;
;; (* 2 (* 2 (* 2 (* 2 (* 2 1)))))
;;
;; (* 2 (* 2 (* 2 (* 2 2))))
;;
;; (* 2 (* 2 (* 2 4)))
;;
;; (* 2 (* 2 8))
;;
;; (* 2 16)
;;
;; 32

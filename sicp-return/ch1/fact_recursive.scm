(define (factorial n)
  (if (= n 1)
    1
    (* n (factorial (- n 1)))))

;; (factorial 6)
;;
;; (* 6 (factorial 5))
;;
;; (* 6 (* 5 (factorial 4)))
;;
;; (* 6 (* 5 (* 4 (factorial 3))))
;;
;; (* 6 (* 5 (* 4 (* 3 (factorial 2)))))
;;
;; (* 6 (* 5 (* 4 (* 3 (* 2 (factorial 1))))))
;;
;; (* 6 (* 5 (* 4 (* 3 (* 2 1)))))
;;
;; (* 6 (* 5 (* 4 (* 3 2))))
;;
;; (* 6 (* 5 (* 4 6)))
;;
;; (* 6 (* 5 24))
;;
;; (* 6 120)
;;
;; 720
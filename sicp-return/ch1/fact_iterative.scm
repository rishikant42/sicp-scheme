(define (fact-iter product counter max-count)
  (if (> counter max-count)
    product
    (fact-iter (* product counter)
               (+ counter 1)
               max-count)))

(define (factorial n)
  (fact-iter 1 1 n))

;; (factorial 6)
;;
;; (fact-iter 1 1 6)
;;
;; (fact-iter 1 2 6)
;;
;; (fact-iter 2 3 6)
;;
;; (fact-iter 6 4 6)
;;
;; (fact-iter 24 5 6)
;;
;; (fact-iter 120 6 6)
;;
;; (fact-iter 720 7 6)
;;
;; 720

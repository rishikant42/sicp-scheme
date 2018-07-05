(define (fact-iter a b count)
  (if (= count 0)
    b
    (fact-iter (+ a b)
               a
               (- count 1))))

(define (fib n)
  (fact-iter 1 0 n))


;; (fib 5)
;;
;; (fact-iter 1 0 5)
;;
;; (fact-iter 1 1 4)
;;
;; (fact-iter 2 1 3)
;;
;; (fact-iter 3 2 2)
;;
;; (fact-iter 5 3 1)
;;
;; (fact-iter 8 5 0)

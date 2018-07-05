(define (square x) (* x x))

(define (expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (expt b (/ n 2))))
        (else (* b (expt b (- n 1))))))

;; space complexity = O(log(n))
;; time complexity = O(log(n))

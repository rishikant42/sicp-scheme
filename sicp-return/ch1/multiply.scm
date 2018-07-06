;; if system doesn't support multiplication operator. It support only addition & substractionoperator

(define (mult-recur a b)
  (if (= b 0)
    0
    (+ a (mult-recur a (- b 1)))))

(define (mult-iter a b)
  (define (iter count result)
    (if (= count 0)
      result
      (iter (- count 1) (+ result a))))
  (iter b 0))

(define (double x) (+ x x))

(define (half x) (/ x 2))

(define (mult-recur-log a b)
  (cond ((= b 0) 0)
        ((even? b) (double (mult-recur-log a (half b))))
        (else (+ a (mult-recur-log a (- b 1))))))

(define (mult-iter-log a b)
  (define (iter a b result)
    (cond ((= b 0) result)
          ((even? b) (iter (double a) (half b) result))
          (else (iter a (- b 1) (+ result a)))))
  (iter a b 0))

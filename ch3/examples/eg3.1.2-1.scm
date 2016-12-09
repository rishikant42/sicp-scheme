(define (rand) (random 10000))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (success-res passed)
  (display "No of trials succeeded = ")
  (display passed)
  (newline))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) (success-res trials-passed) (/ trials-passed trials))
          ((experiment) (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

;; TESTS:
;; 
;; 1 ]=> (estimate-pi 10000)
;; No of trials succeeded = 6009
;; ;Value: 3.1599086167641857
;; 
;; 1 ]=> (estimate-pi 10000)
;; No of trials succeeded = 6085
;; ;Value: 3.140113412477491

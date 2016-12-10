(define (random-in-range low high) 
  (let ((range (- high low))) 
    (+ low (random range))))
 
(define (estimate-integral p x1 y1 x2 y2 trials)
  (define area (abs (* (- x2 x1) (- y2 y1))))          ;; Area-of-rect = 36
  (define (exprmnt)
    (p (random-in-range x1 x2)
       (random-in-range y1 y2)))
  (* area (monte-carlo trials exprmnt)))

(define (pt-lie-in-circle? x y)
  (<= (+ (square (- x 5)) (square (- y 7))) (square 3)))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) (/ trials-passed trials))
          ((experiment) (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

;; TESTS
;; 
;; Actual area of circle = pi * r * r = 3.14 * 3 3 = 28.27
;; 
;; By monte-carlo-method
;; 
;; 1 ]=> (estimate-integral pt-lie-in-circle? 2.0 4 8 10 100)
;; 
;; ;Value: 27.36


(define (estimate-pi)
  (/ (estimate-integral pt-lie-in-circle? 2.0 4 8 10 100) 9))

;; 1 ]=> (estimate-pi)
;;
;; ;Value: 3.16

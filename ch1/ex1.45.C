(define tolerance 0.00001)
(define (average a b) (/ (+ a b) 2.0))

(define (close-enough? a b)
  (< (abs (- a b)) tolerance))

(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(define (newton-transform g)
  (lambda (x) (- x 
                 (/ (g x)
                    ((deriv g) x)))))

(define (average-damping f)
  (lambda (x) (average x (f x))))

(define (exp b n)
  (if (= n 0)
    1
    (* b (exp b (- n 1)))))

;; (define (exp2 b n)
;;   (define (iter counter result)
;;     (if (= counter 0)
;;       result
;;       (iter (- counter 1) (* b result))))
;;   (iter n 1))

;; (define (exp3 b n)
;;   (cond ((= n 0) 1)
;;         ((even? n) (square (exp3 b (/ n 2))))
;;         (else (* b (exp3 b (- n 1))))))
;; 
;; (define (exp4 b n)
;;   (cond ((= n 0) 1)
;;         ((even? n) (exp4 (square b) (/ n 2)))
;;         (else (* b (exp4 b (- n 1))))))

(define (sqrt x)
  (fixed-point (average-damping (lambda (y) (/ x y)))
               1.0))

(define (cube-root x)
  (fixed-point (average-damping (lambda (y) (/ x (square y))))
               1.0))

(define (quad-root x)
  (fixed-point (average-damping (average-damping (lambda (y) (/ x (exp y 3)))))
               1.0))

(define (penta-root x)
  (fixed-point (average-damping (average-damping (lambda (y) (/ x (exp y 4)))))
               1.0))

(define (hexa-root x)
  (fixed-point (average-damping (average-damping (lambda (y) (/ x (exp y 5)))))
               1.0))

(define (svn-root x)
  (fixed-point (average-damping (average-damping (lambda (y) (/ x (exp y 6)))))
               1.0))

(define (egh-root x)
  (fixed-point (average-damping (average-damping (average-damping (lambda (y) (/ x (exp y 7))))))
               1.0))

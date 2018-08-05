(define (make-interval a b) (cons a b))

(define (lower-bound i) (min (car i) (cdr i)))

(define (upper-bound i) (max (car i) (cdr i)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

;;(define (div-interval x y)
;;  (mul-interval x
;;                (make-interval (/ 1 (upper-bound y))
;;                               (/ 1 (lower-bound y)))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (width i) (/ (+ (lower-bound i) (upper-bound i)) 2.0))

;; ex 2.8
;; 1 ]=> (define p1 (make-interval 2 6))
;;
;; ;Value: p1
;;
;; 1 ]=> (define p2 (make-interval 4 10))                                                                    [0/374]
;;
;; ;Value: p2
;;
;;  ]=> (width p2)
;;
;; ;Value: 7.
;;
;; 1 ]=> (width p1)
;;
;; ;Value: 4.
;;
;; 1 ]=> (width (add-interval p1 p2))
;;
;; ;Value: 11.
;;
;; 1 ]=> (width (sub-interval p1 p2))
;;
;; ;Value: -3.
;;
;; 1 ]=> (width (mul-interval p1 p2))
;;
;; ;Value: 34.
;;
;; 1 ]=> (width (div-interval p1 p2))
;;
;; ;Value: .85

(define (div-interval x y)
  (if (>= 0 (* (lower-bound y) (upper-bound y)))
    (error "Division error (interval spans 0)" y)
    (mul-interval x
                  (make-interval (/ 1. (upper-bound y))
                                 (/ 1. (lower-bound y))))))


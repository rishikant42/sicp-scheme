(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

;;;;;;;;;;;;;; Rectangular form constructors & selectors ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-from-real-imag x y)
  (cons x y))

(define (make-from-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a))))

(define (real-part z) (car z))

(define (imag-part z) (cdr z))

(define (magnitude z)
  (sqrt (+ (square (real-part z)) (square (imag-part z)))))

(define (angle z)
  (atan (imag-part z) (real-part z)))


;;;;;;;;;;;;;;;;;;;; TESTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1 ]=> (define z1 (make-from-real-imag 4 5))
;; 
;; ;Value: z1
;; 
;; 1 ]=> (define z2 (make-from-real-imag 2 3))
;; 
;; ;Value: z2
;; 
;; 1 ]=> (add-complex z1 z2)
;; 
;; ;Value 18: (6 . 8)
;; 
;; 1 ]=> (mul-complex z1 z2)
;; 
;; ;Value 19: (-6.999999999999996 . 22.)
;; 
;; 
;; 1 ]=> (define r (magnitude z1))
;; 
;; ;Value: r
;; 
;; 1 ]=> (define a (angle z1))
;; 
;; ;Value: a
;; 
;; 1 ]=> r
;; 
;; ;Value: 6.4031242374328485
;; 
;; 1 ]=> a
;; 
;; ;Value: .8960553845713439
;; 
;; 1 ]=> (make-from-mag-ang 6.40 .896)
;; 
;; ;Value 20: (3.998325085955702 . 4.9973389425790735)            == z1

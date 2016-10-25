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

;;;;;;;;;;;;;; Polar form constructors & selectors ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-from-mag-ang r a) (cons r a))

(define (make-from-real-imag x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))

(define (magnitude z) (car z))

(define (angle z) (cdr z))

(define (real-part z)
  (* (magnitude z) (cos (angle z))))

(define (imag-part z)
  (* (magnitude z) (sin (angle z))))

;;;;;;;;;;;;;;;;;;;; TESTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Here using same complex no. as in prvs example (but in form of magnitude and angle)
;; 
;; 1 ]=> (define z1 (make-from-mag-ang 6.4031 .8961))
;; 
;; ;Value: z1
;; 
;; 1 ]=> (define z2 (make-from-mag-ang 3.61 .9827))
;; 
;; ;Value: z2
;; 
;; 1 ]=> (add-complex z1 z2)
;; 
;; ;Value 22: (10.004445319998963 . .9273147338134813)
;; 
;; 1 ]=> (real-part (add-complex z1 z2))
;; 
;; ;Value: 6.0025109949581035
;; 
;; 1 ]=> (imag-part (add-complex z1 z2))
;; 
;; ;Value: 8.00367340139865
;; 
;; 1 ]=> (mul-complex z1 z2)
;; 
;; ;Value 23: (23.115191 . 1.8788)
;; 
;; 1 ]=> (real-part (mul-complex z1 z2))
;; 
;; ;Value: -7.00752874046689
;; 
;; ;1 ]=> (imag-part (mul-complex z1 z2))
;; 
;; ;Value: 22.027405564841526
;; 
;; 1 ]=> (real-part z1)
;; 
;; ;Value: 3.9997617787158735
;; 
;; ;1 ]=> (imag-part z1)
;; ;
;; ;Value: 5.000159529807388
;; 
;; ;1 ]=> (make-from-real-imag 3.999 5.0000)
;; 
;; ;Value 24: (6.402499590003892 . .8961773476890922)           ==  z1

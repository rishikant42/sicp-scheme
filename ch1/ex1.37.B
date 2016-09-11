(define (cont-frac1 n d k)   ;; Iterative process, exclude last term
  (define (iter i result)
    (if (= i 0)
      result
      (iter (- i 1)
            (/ (n i)
               (+ (d i) result)))))
  (iter k 0))

(define (cont-frac2 n d k)     ;; Iterative procee, include last term
  (define (iter i result)
    (if (= i 0)
      (/ (n i) (+ (d i) result))
      (iter (- i 1)
            (/ (n i)
               (+ (d i) result)))))
  (iter k 0))

(define (cont-frac3 n d k)      ;; recursive process, exclude last term
  (define (recur i)
    (if (= i 0)
      0
      (/ (n i) (+ (d i) (recur (- i 1))))))
  (recur k))

(define (cont-frac4 n d k)      ;; recursive process, include last term
  (define (recur i)
    (if (= i 0)
      (/ (n i) (d i))
      (/ (n i) (+ (d i) (recur (- i 1))))))
  (recur k))

(define (cont-frac5 n d k)    
  (define (recur i)
    (if (= i k)
      0
      (/ (n i) (+ (d i) (recur (+ i 1))))))
  (recur 0))

(define (cont-frac6 n d k)
  (define (recur i)
    (if (= i k)
      (/ (n i) (d i))
      (/ (n i) (+ (d i) (recur (+ i 1))))))
  (recur 0))

(define (cont-frac7 n d k)     ;; best-one 
  (define (recur i)
    (if (> i k)
      0
      (/ (n i) (+ (d i) (recur (+ i 1))))))
  (recur 1))

(define (cont-frac8 n d k)     ;; same as cont-frac7 procedure 
  (define (recur i)
    (if (= i k)
      (/ (n i) (d i))
      (/ (n i) (+ (d i) (recur (+ i 1))))))
  (recur 1))

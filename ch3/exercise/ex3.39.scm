(load "parallel.scm")

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
               (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell)
  (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell)
    true
    (begin (set-car! cell true)
           false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run-many-times in-test num) 
  (define (run-test numtimes output) 
    (let ((m (in-test))) 
      (cond ((= 0 numtimes) output) 
            ((memq m output) (run-test (- numtimes 1) output)) 
            (true (run-test (- numtimes 1) (cons m output)))))) 
  (run-test num '())) 

(define (test-2) 
  (define x 10) 
  (define s (make-serializer)) 
  (parallel-execute (lambda () (set! x ((s (lambda () (* x x)))))) 
                    (s (lambda () (set! x (+ x 1))))) 
  x) 

(display (run-many-times test-2 10000)) 

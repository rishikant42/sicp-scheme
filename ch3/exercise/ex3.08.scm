(define (fn)
  (let ((first #t))
    (lambda (x)
      (if (and first (= x 1))
        (begin (set! first #f)
               1)
        (begin (set! first #f)
               0)))))

(define f (fn))

;; 1 ]=> (load "ex3.08.scm")
;; 
;; ;Loading "ex3.08.scm"... done
;; ;Value: f
;; 
;; 1 ]=> (f 0)
;; 
;; ;Value: 0
;;                                
;; 1 ]=> (f 1)
;; 
;; ;Value: 0


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; RESTART REPL ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; 1 ]=> (load "ex3.08.scm")
;; 
;; ;Loading "ex3.08.scm"... done
;; ;Value: f
;; 
;; 1 ]=> (f 1)
;; 
;; ;Value: 1
;;                                
;; 1 ]=> (f 0)
;; 
;; ;Value: 0

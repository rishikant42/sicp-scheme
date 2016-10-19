(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records))) (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

(define (key x) (car x))

;; 1 ]=> (define record '((:a 1) (:b 2) (:c 3) (:d 4) (:e 5)))
;; 
;; ;Value: record
;; 
;; 1 ]=> (lookup ':c record)
;; 
;; ;Value 11: (:c 3)
;; 
;; 1 ]=> (lookup ':f record)
;; 
;; ;Value: #f

(define (map1 proc items)
  (if (null? items)
    '()
    (cons (proc (car items))
          (map1 proc (cdr items)))))

(define (gen-map proc . args)
  (if (null? (car args))
    '()
    (cons (apply proc (map1 car args))
          (apply gen-map (cons proc (map1 cdr args))))))


;; (gen-map + '(1 2 3) '(4 5 6))
;; 
;; (cons (apply + (map1 car '((1 2 3) (4 5 6))))
;;       (apply gen-map (cons proc (map1 cdr '((2 3) (5 6))))))
;; 
;; (cons (apply + '(1 4))
;;       (apply gen-map (cons + '((2 3) (5 6)))))
;; 
;; (cons 5
;;       (apply gen-map (+ '(2 3) '(5 6))))
;; 
;; (cons 5 
;;       (gen-map + '(2 3) '(5 6)))

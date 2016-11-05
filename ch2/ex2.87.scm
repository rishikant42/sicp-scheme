;; add into polynomial package
(define (=zero? x)
  (cond ((number? x) (= x 0))
        ((pair? x) false)
        (else (error "Unknown type"))))

(put '=zero? 'polynomial =zero?)

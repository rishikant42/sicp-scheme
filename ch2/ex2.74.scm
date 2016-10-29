(define (type-tag file) 
  (car file)) 

(define (apply-generic op name file) 
  (let ((division-name (type-tag file))) 
    (let ((proc (get op division-name))) 
      (if proc 
        (proc name file) 
        (error "no result")))))

(define (install-division-1-package) 
  ;;internal procedures 
  (define (get-record name file) 
    (cond ((null? file) (error "no result")) 
          ((eq? name (get-name (cadr file))) (cons (cadr file) 
                                                   (get-record name (cdr file)))) 
          (else (get-record name (cdr file))))) 
  (define (get-name record) 
    (car record)) 

  ;;interface to the rest of the system 
  (put 'get-record 'division-1 get-record) 
  (put 'get-name 'division-1 get-name) 
  'done) 

(define (get-record name file) 
  (apply-generic 'get-record name file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-salary name file) 
  (cond ((null? file) (error "no result")) 
        ((eq? name (get-name (cadr file))) (cons (cadr (cadr file)) 
                                                 (get-salary name (cdr file)))) 
        (else (get-salary name (cdr file))))) 

(put 'get-salary 'division-1 get-salary) 

(define (get-salary name file) 
  (apply-generic 'get-salary name file))

(define (find-employee-record name list) 
  (if (null? list) 
    (error "no result") 
    (append (get-record (car list)) 
            (find-employee-record name (cdr list)))))

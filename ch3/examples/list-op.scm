(define nil '())

(define (enumerate-interval a b)
  (if (> a b)
    nil
    (cons a (enumerate-interval (+ a 1) b))))

(define (filter pred? items)
  (cond ((null? items) nil)
        ((pred? (car items)) (cons (car items) (filter pred? (cdr items))))
        (else (filter pred? (cdr items)))))

(define (accumulate op init items)
  (if (null? items)
    init
    (op (car items)
        (accumulate op init (cdr items)))))

(define (map proc items)
  (if (null? items)
    nil
    (cons (proc (car items))
          (map proc (cdr items)))))

(define (for-each op items)
  (cond ((null? items) 'done)
        (else (op (car items))
              (for-each op (cdr items)))))

(define (display-items i)
  (for-each display-line i))

(define (display-line x)
  (newline)
  (display x))

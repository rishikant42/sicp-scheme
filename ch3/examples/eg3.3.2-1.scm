(define (front-ptr queue) (car queue))

(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item) (set-car! queue item))

(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (make-queue) (cons '() '()))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (front-queue queue)
  (if (empty-queue? queue)
    (error "FRONT called with an empty queue" queue)
    (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (if (empty-queue? queue)
      (begin (set-front-ptr! queue new-pair)
             (set-rear-ptr! queue new-pair)
             queue)
      (begin (set-cdr! (rear-ptr queue) new-pair)
             (set-rear-ptr! queue new-pair)
             queue))))

(define (delete-queue! queue)
  (if (empty-queue? queue)
    (error "DELETE! called with an empty queue" queue)
    (begin (set-front-ptr! queue (cdr (front-ptr queue)))
           queue)))

;; TEST
;; 
;; 1 ]=> (define q (make-queue))
;; 
;; ;Value: q
;; 
;; 1 ]=> (insert-queue! q 'a)
;; 
;; ;Value 11: ((a) a)
;; 
;; 1 ]=> (insert-queue! q 'b)
;; 
;; ;Value 11: ((a b) b)
;; 
;; 1 ]=> (insert-queue! q 'c)
;; 
;; ;Value 11: ((a b c) c)
;; 
;; 1 ]=> (front-queue q)
;; 
;; ;Value: a
;; 
;; 1 ]=> (delete-queue! q )
;; 
;; ;Value 11: ((b c) c)
;; 
;; 1 ]=> (delete-queue! q)
;; 
;; ;Value 11: ((c) c)
;; 
;; 1 ]=> (delete-queue! q)
;; 
;; ;Value 11: (() c)
;; 
;; 1 ]=> (insert-queue! q 'a)
;; 
;; ;Value 11: ((a) a)

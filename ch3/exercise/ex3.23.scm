(define (front-ptr deque) (car deque))

(define (rear-ptr deque) (cdr deque))

(define (set-front-ptr! deque item) (set-car! deque item))

(define (set-rear-ptr! deque item) (set-cdr! deque item))

(define (make-deque) (cons '() '()))

(define (empty-deque? deque) (null? (front-ptr deque)))

(define (front-deque deque)
  (if (empty-deque? deque)
    (error "FRONT call with an empty queue" deque)
    (car (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
    (error "FRONT call with an empty queue" deque)
    (car (rear-ptr deque))))

(define (rear-insert-deque deque item)
  (let ((new-pair (cons item '())))
    (if (empty-deque? deque)
      (begin (set-front-ptr! deque new-pair)
             (set-rear-ptr! deque new-pair)
             deque)
      (begin (set-cdr! (rear-ptr deque) new-pair)
             (set-rear-ptr! deque new-pair)
             deque))))

(define (front-insert-deque deque item)
  (let ((new-pair (cons item '())))
    (if (empty-deque? deque)
      (begin (set-front-ptr! deque new-pair)
             (set-rear-ptr! deque new-pair)
             deque)
      (begin (set-cdr! new-pair (front-ptr deque))
             (set-front-ptr! deque new-pair)
             deque))))

(define (front-delete-deque deque)
  (if (empty-deque? deque)
    (error "DELETE call with an empty queue" deque)
    (begin (set-front-ptr! deque (cdr (front-ptr deque)))
           deque)))

(define (second-last-pair items)
  (if (null? (cddr items))
    items
    (second-last-pair (cdr items))))

(define (rear-delete-deque deque)
  (if (empty-deque? deque)
    (error "DELETE call with an empty queue" deque)
    (begin (set-rear-ptr! deque (second-last-pair (front-ptr deque)))
           (set-cdr! (rear-ptr deque) '())
           deque)))

;; All operation take take O(1) constant time, except rear-delete-deque. It take O(n) time where n is length of queue
;; bcoz it need to successive cadering down
;; if we want to implement all operation in O(1) time, we have to maintain two ptr at each node (insted of 1)
;; queue will look like in such case
;;
;;                ----------------------------------- 
;;                |         |             |         |
;;                | prvs_ptr| node_value  | nxt_ptr |
;;                |         |             |         |
;;                -----------------------------------

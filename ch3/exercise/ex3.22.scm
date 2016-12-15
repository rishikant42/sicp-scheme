(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))

    (define (empty-queue?)
      (null? front-ptr))

    (define (front-queue)
      (if (empty-queue?)
        (error "FRONT call with an empty queue")
        (car front-ptr)))

    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (if (empty-queue?)
          (begin (set! front-ptr new-pair)
                 (set! rear-ptr new-pair))
          (begin (set-cdr! rear-ptr new-pair)
                 (set! rear-ptr new-pair)))))

    (define (delete-queue!)
      (if (empty-queue?)
        (error "DELETE! call with an empty queue")
        (set! front-ptr (cdr front-ptr))))

    (define (print-queue) front-ptr)

    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) empty-queue?)
            ((eq? m 'front-queue) front-queue)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            ((eq? m 'print-queue) print-queue)
            (else (error "Unknown request -- DISPATCH"))))

    dispatch))

(define (empty-queue? q)
  ((q 'empty-queue?)))

(define (insert-queue! q item)
  ((q 'insert-queue!) item)
  q)

(define (delete-queue! q)
  ((q 'delete-queue!))
  q)

(define (front-queue q)
  ((q 'front-queue)))

(define (print-queue q)
  ((q 'print-queue)))

(define (list-ref items n)
  (if (= n 0)
    (car items)
    (list-ref (cdr items) (- n 1))))

(define (length-recur items)
  (if (null? items)
    0
    (+ 1 (length-recur (cdr items)))))

(define (length-iter items)
  (define (iter seq result)
    (if (null? seq)
      result
      (iter (cdr seq) (+ result 1))))
  (iter items 0))

(define (append-recur list1 list2)
  (if (null? list1)
    list2
    (cons (car list1)
          (append-recur (cdr list1) list2))))

(define (last-pair items)
  (if (null? (cddr items))
    (cdr items)
    (last-pair (cdr items))))

(define (last-pair2 items)
  (if (= (length items) 1)
    items
    (last-pair2 (cdr items))))

(define (last-pair3 items)
  (if (null? (cdr items))
    items
    (last-pair3 (cdr items))))

(define (reverse items)
  (define (iter seq result)
    (if (null? seq)
      result
      (iter (cdr seq) (cons (car seq) result))))
  (iter items '()))

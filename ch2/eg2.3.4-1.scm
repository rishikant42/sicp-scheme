(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

(define a (make-leaf 'a 8))
(define b (make-leaf 'b 3))
(define c (make-leaf 'c 1))
(define d (make-leaf 'd 1))
(define f (make-leaf 'f 1))
(define e (make-leaf 'e 1))
(define g (make-leaf 'f 1))
(define h (make-leaf 'g 1))

;; 1 ]=> (define t1 (make-code-tree c d))
;; 
;; ;Value: t1
;; 
;; 1 ]=> (define t2 (make-code-tree e f))
;; 
;; ;Value: t2
;; ;
;; 1 ]=> (define t3 (make-code-tree g h))
;; 
;; ;Value: t3
;; 
;; 1 ]=> (define t5 (make-code-tree t2 t3))
;; 
;; ;Value: t5
;; 
;; 1 ]=> (define t6 (make-code-tree b t1))
;; 
;; ;Value: t6
;; ;
;; 1 ]=> (define t7 (make-code-tree t6 t5))
;; 
;; ;Value: t7
;; 
;; 1 ]=> (define t8 (make-code-tree a t7))
;; 
;; ;Value: t8
;; 
;; 1 ]=> t8
;;;Value 19: ((leaf a 8) (((leaf b 3) ((leaf c 1) (leaf d 1) (c d) 2) (b c d) 5) (((leaf e 1) (leaf f 1) (e f) 2) ((leaf f 1) (leaf g 1) (f g) 2) (e f f g) 4) (b c d e f f g) 9) (a b c d e f f g) 17)
;;;

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      '()
      (let ((next-branch (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch)
                (decode-1 (cdr bits) tree))
          (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

;; 1 ]=> (decode '(1 0 0 0 1 0 1 0) t8)
;; 
;; ;Value 21: (b a c)
;;
;;
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair) (cadr pair))
                  (make-leaf-set (cdr pairs))))))

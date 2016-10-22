(define a (make-leaf 'a 8))
(define b (make-leaf 'b 3))
(define c (make-leaf 'c 1))
(define d (make-leaf 'd 1))
(define f (make-leaf 'f 1))
(define e (make-leaf 'e 1))
(define g (make-leaf 'f 1))
(define h (make-leaf 'g 1))

(define t1 (make-code-tree c d))

(define t2 (make-code-tree e f))

(define t3 (make-code-tree g h))

(define t5 (make-code-tree t2 t3))

(define t6 (make-code-tree b t1))

(define t7 (make-code-tree t6 t5))

(define t8 (make-code-tree a t7))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))

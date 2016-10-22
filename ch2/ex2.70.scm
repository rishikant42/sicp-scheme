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

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      '()
      (let ((next-branch (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
          (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair) (cadr pair))
                  (make-leaf-set (cdr pairs))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

(define (exist? symbol tree)
  (define (helper items)
    (cond ((null? items) false)
          ((equal? (car items) symbol) true)
          (else (helper (cdr items)))))
  (helper (symbols tree)))

(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '())
        ((exist? symbol tree)
         (if (exist? symbol (left-branch tree)) 
           (cons 0 (encode-symbol symbol (left-branch tree)))
           (cons 1 (encode-symbol symbol (right-branch tree)))))
        ((error "symbol doesn't present in tree" symbol))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (generate-huffman-tree pairs)
   (successive-merge (make-leaf-set pairs)))
  
 (define (successive-merge leaf-set) 
   (if (= (length leaf-set) 1) 
     (car leaf-set) 
     (let ((first (car leaf-set)) 
           (second (cadr leaf-set)) 
           (rest (cddr leaf-set))) 
       (successive-merge (adjoin-set (make-code-tree first second) 
                                     rest))))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;EX2.70;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define symbol-frequency '((a 2) (boom 1) (get 2) (job 2) (na 16) (sha 3) (yip 9) (wah 1)))
;; 
;; 1 ]=> (generate-huffman-tree symbol-frequency)
;; 
;; ;Value 11: ((leaf na 16) ((leaf yip 9) (((leaf a 2) ((leaf wah 1) (leaf boom 1) (wah boom) 2) (a wah boom) 4) ((leaf sha 3) ((leaf job 2) (leaf get 2) (job get) 4) (sha job get) 7) (a wah boom sha job get) 11) (yip a wah boom sha job get) 20) (na yip a wah boom sha job get) 36)
;; ;
;; 
;; EXPLANATION: 
;; 
;; 1 ]=> (define s1 (make-leaf-set symbol-frequency))
;; 
;; ;Value: s1
;; 
;; 1 ]=> (define s2 (adjoin-set (make-code-tree (car s1) (cadr s1)) (cddr s1)))
;; 
;; ;Value: s2
;; 
;; 1 ]=> (define s3 (adjoin-set (make-code-tree (car s2) (cadr s2)) (cddr s2)))
;; 
;; ;Value: s3
;; 
;; 1 ]=> (define s4 (adjoin-set (make-code-tree (car s3) (cadr s3)) (cddr s3)))
;; 
;; ;Value: s4
;; 
;; 1 ]=> (define s5 (adjoin-set (make-code-tree (car s4) (cadr s4)) (cddr s4)))
;; 
;; ;Value: s5
;; 
;; 1 ]=> (define s6 (adjoin-set (make-code-tree (car s5) (cadr s5)) (cddr s5)))
;; 
;; ;Value: s6
;; 
;; 1 ]=> (define s7 (adjoin-set (make-code-tree (car s6) (cadr s6)) (cddr s6)))
;; 
;; ;Value: s7
;; 
;; 1 ]=> (define s8 (adjoin-set (make-code-tree (car s7) (cadr s7)) (cddr s7)))
;; 
;; ;Value: s8
;; 
;; 1 ]=> (define result (car s8))
;; 
;; ;Value: result
;; 
;; 1 ]=> (equal? (generate-huffman-tree symbol-frequency) result)
;; 
;; ;Value: #t


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1 ]=> (define song '(get a job sha na na na na na na na na get a job sha na na na na na na na na wah yip yip yip yip yip yip yip yip yip sha boom))
;; 
;; ;Value: song
;; 
;; 1 ]=> (define tree (generate-huffman-tree symbol-frequency))
;; 
;; ;Value: tree
;; 
;; 1 ]=> (encode song tree)
;; 
;; ;Value 12: (1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 1)
;; ;
;; 1 ]=> (length (encode song tree))
;; 
;; ;Value: 84

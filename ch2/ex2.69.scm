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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TESTS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1 ]=> (generate-huffman-tree '((a 4) (b 2) (c 1) (d 1)))
;; 
;; ;Value 11: ((leaf a 4) ((leaf b 2) ((leaf d 1) (leaf c 1) (d c) 2) (b d c) 4) (a b d c) 8)
;; 
;; 1 ]=> (define tree (generate-huffman-tree '((a 4) (b 2) (c 1) (d 1))))
;; 
;; ;Value: tree
;; 
;; 1 ]=> (decode '(0 1 1 0 0 1 0 1 0  1 1 1 0) tree)
;; 
;; ;Value 12: (a d a b b c a)
;; 
;; 1 ]=> (encode (decode '(0 1 1 0 0 1 0 1 0  1 1 1 0) tree) tree)
;; 
;; ;Value 13: (0 1 1 0 0 1 0 1 0 1 1 1 0)

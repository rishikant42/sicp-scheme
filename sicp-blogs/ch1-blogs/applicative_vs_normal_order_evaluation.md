# Applicative vs normal order evaluation

### Prerequisite

---
- *Primitive expressions*: Inbuilt data/procedure in any programming language. Their defination is predifened. The values of built-in operators are the machine instruction sequences that carry out the corresponding operations.  Eg: \*, +, 10

- *Compound expressions*: These are build by combining primitive expressions.

- *Abstraction*: A mean by which compound elements can be named and manipulated as units. Eg: `define` keyword
---

### How interpreter evaluate the expressions?

---

Usually interpreter follow these steps to evaluate the expressions:

- Evaluate the subexpressions of the combination.
- Apply the procedure that is the value of the leftmost subexpression (the operator) to the arguments that are the values of the other subexpressions (the operands).

---

### Example:

---
Take an example to explain the different method of evaluation. Lets define a compound proceduce as given below

```
(define (square x)
  (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2))

```
---


### Applicative order evaluation:

---

- As described above, evaluates the operator and operands and then applies the resulting procedure to the resulting arguments.

- In short, evaluate the arguments and then apply.

```

(f 5)

(sum-of-squares (+ 5 1) (* 5 2))

(sum-of-squares 6 10)

(+ (square 6) (square 10))

(+ (* 6 6) (* 10 10))

(+ 36 100)

136

```

---

### Normal order evaluation

---

- An alternative evaluation model would not evaluate the operands until their values were needed. Instead it would first substitute operand expressions for parameters until it obtained an expression involving only primitive operators, and would then perform the evaluation.

- In short, fully expand and then reduce.

```
(f 5)

(sum-of-squares (+ 5 1) (* 5 2))

(+ (square (+ 5 1)) (square (* 5 2)))

(+ (* (+ 5 1) (+ 5 1)) (* (* 5 2) (* 5 2)))         ;; only primitives

(+ (* 6 6) (* 10 10))

(+ 36 100)

136

```
- This gives the same answer as our previous evaluation model, but the process is different. In particular, the evaluations of (+ 5 1) and (\* 5 2) are each performed twice here, corresponding to the reduction of the expression (\* x x) with x replaced respectively by (+ 5 1) and (\* 5 2).
---

### NOTE: Lisp uses applicative-order evaluation, partly because of the additional efficiency obtained from avoiding multiple evaluations of same expressions.

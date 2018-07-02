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



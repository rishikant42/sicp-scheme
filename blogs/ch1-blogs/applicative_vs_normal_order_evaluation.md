# Applicative vs normal order evaluation

### How interpreter evalute the expressions?

---

Usually interpreter follow below steps to evalute the expression:

- Evaluate the subexpressions of the combination.
- Apply the procedure that is the value of the leftmost subexpression (the operator)
to the arguments that are the values of the other subexpressions (the operands).

---

# Procedures and the processes they Generate

**Recursive procedures**: Procedures that are defined in terms of itself.

*Example*: Factorial function can be defined as

n! = n * (n - 1) * (n - 2) ......... 3 * 2 * 1

---
## Methods to calculate the factorial

### Method1: 
n! is equal to n times (n - 1)! for any positive integer n. Hence, n! = n * (n - 1)!

**Scheme code**

```
(define (factorial n)
  (if (= n 1)
    1
    (* n (factorial (- n 1)))))
```

Let's try to see how interpreter evaluate the computation by this method. 

```
;; A linear recursive process for computing 6!

(factorial 6)

(* 6 (factorial 5))

(* 6 (* 5 (factorial 4)))

(* 6 (* 5 (* 4 (factorial 3))))

(* 6 (* 5 (* 4 (* 3 (factorial 2)))))

(* 6 (* 5 (* 4 (* 3 (* 2 (factorial 1))))))

(* 6 (* 5 (* 4 (* 3 (* 2 1)))))

(* 6 (* 5 (* 4 (* 3 2))))

(* 6 (* 5 (* 4 6)))

(* 6 (* 5 24))

(* 6 120)

720
```

### Method2:

We can describe a rule for computing n! by specifying that we first multiply 1 by 2, then multiply the result by 3, then by 4, and so on until we reach n. 

product <--- counter * product
counter <--- counter + 1

More formally, we maintain a running product, together with a counter that counts from 1 up to n. We can describe the computation by saying that the counter and the product simultaneously change from one step to the next according to the above rules. And we return value of the product when the counter exceeds n.

# Procedures and the processes they generate

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

product <--- counter \* product \
counter <--- counter + 1

We maintain a running product, together with a counter that counts from *1* up to *n*. We can describe the computation by saying that the counter and the product simultaneously change from one step to the next according to the above rules. And we return value of the product when the counter exceeds *n*.

**Scheme code**
```
(define (fact-iter product counter max-count)
  (if (> counter max-count)
    product
    (fact-iter (* product counter)
               (+ counter 1)
               max-count)))

(define (factorial n)
  (fact-iter 1 1 n))
```

Let’s try to see how interpreter evaluate the computation by this method.

```
;; A linear iterative process for computing 6!.

(factorial 6)

(fact-iter 1 1 6)

(fact-iter 1 2 6)

(fact-iter 2 3 6)

(fact-iter 6 4 6)

(fact-iter 24 5 6)

(fact-iter 120 6 6)

(fact-iter 720 7 6)

720
```

**Observations:**

Compare above two processes. From one point of view, they seem hardly different at all. Both compute the same mathematical function on the same domain, and each requires a number of steps proportional to *n* to compute *n!*. Both processes even carry out the same sequence of multiplications, obtaining the same sequence of partial products. On the other hand, when we consider the `shapes` of the two processes, we find that they evolve quite differently.

### Recursive process:

Consider the shape of first process, It's shape first grow then shrink. The expansion occurs as the process builds up a chain of *deferred operations* and the contraction occurs as the operations are actually performed. This type of process, characterized by a chain of *deferred operations*, is called a recursive process.

**Deferred operations**: The operator along which shape grow & srink. In *n!* calculation, it's multiplication operator.

**General syntax**:
```
(define (<fn-name> <params>)
  (if (<some-condition>)
    <return-value>
    (<deferred operator> (<fn-name> <updated-params>))))            ;; this line syntax make process recursive
```

**Note:** Carrying out recursive process requires that the interpreter keep track of the operations to be performed later on. In the computation of n!, the length of the chain of deferred multiplications, and hence the amount of information needed to keep track of it, grows linearly with n (is proportional to n), just like the number of steps. Such a process is called a *linear recursive process*.

### Iterative process:

Consider the shapee of second process. It doesn't grow and srink. At each step, all we need to keep track of, for any *n*, are the current values of the variables *product, counter, and max-count*. We call this an iterative process.

**General syntax:**
```
(define (<fn-name> <params>)
  (if (<some-condition)
    <return-value>
    (<fn-name> <updated-params>)))      ;; this line syntax make process iterative
```

**NOTE:**  In general, an iterative process is one whose state can be summarized by a fixed number of state variables, together with a fixed rule that describes how the state variables should be updated as the process moves from state to state and an (optional) end test that specifies conditions under which the process should terminate. In computing n!, the number of steps required grows linearly with n. Such a process is called a linear iterative process.

### Observations:
- *Recursive procedures* can be defined as *recursive process* and *iterative process*.

- When we describe a procedure as recursive, we are referring to the syntactic fact that the procedure definition refers (either directly or indirectly) to the procedure itself. But when we describe a process as following a pattern that is, say, linearly recursive, we are speaking about how the process evolves, not about the syntax of how a procedure is written.

- In case of iterative process,  An interpreter need to keep track of only fixed variables in order to execute the process. In *fact-iter* process, these variables are *product, counter and max-count*.

- In case of recursive process, some additional hidden information maintained by the interpreter that allow to perform actual computation later(At time of srink). This process required auxiliary memory (usually stack). Process grow and srink over stack.

### Tail call optimization:

When recursive procedure is defined in terms of iterative process, it is known as *tail recursion* and optimization is known as *tail call optimization*. Tail recursion is well known compiler optimization trick.

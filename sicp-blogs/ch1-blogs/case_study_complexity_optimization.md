# Algorithmic complexities

Sometimes, there are more than one way to solve a problem. We should compare the performance different algorithms and choose the best one to solve a particular problem. For any algorithm, we mostly consider **time complexity** and **space complexity**. 

*Time complexity* of an algorithm quantifies the amount of time taken by an algorithm to run as a function of the length of the input. It describes the amount of time it takes to run an algorithm. It also describes, how time will vary as we change the size of input.

Similarly, *space complexity* of an algorithm quantifies the amount of space or memory taken by an algorithm to run as a function of the length of the input.

Mostly **asymptotic notation** is used to describe algorithmic complexities.

**Big-O-notation:**
It is used to describes the time required in the worst-case scenario. It gives the upper bound time for any algorithm.

**Big-Omega-notation:**
It is used to describes the time required in the best-case scenario. It gives the lower bound time for any algorithm.

**Big-Theta-notation:**:
It gives the both upper & lower bound time for any algorithm.

---

### Example: 

**Exponentiation calculation**. Base *b*, power *n* ==> b^n

**Defination:**
```
b^n  = b * b^(n-1)
b^0 = 1
```

---
**Method1:** Recursive process

**Scheme procedure:**
```
(define (expt b n)
  (if (= n 0)
    1
    (* b (expt b (- n 1)))))
```

**Evaluation:**

```
(expt 2 5)

(* 2 (expt 2 4))

(* 2 (* 2 (expt 2 3)))

(* 2 (* 2 (* 2 (expt 2 2))))

(* 2 (* 2 (* 2 (* 2 (expt 2 1)))))

(* 2 (* 2 (* 2 (* 2 (* 2 (expt 2 0))))))

(* 2 (* 2 (* 2 (* 2 (* 2 1)))))

(* 2 (* 2 (* 2 (* 2 2))))

(* 2 (* 2 (* 2 4)))

(* 2 (* 2 8))

(* 2 16)

32
```

**complexities:**
```
time = Theta(n)
space = Theta(n)
```

This is a linear recursive process, which requires *Theta(n)* steps and *Theta(n)* space. Can we improve our solution?

**Method2:** Iterative process

**Scheme procedure:**
```
(define (expt-iter b counter product)
  (if (= counter 0)
    product
    (expt-iter b
               (- counter 1)
               (* b product))))


(define (expt b n)
  (expt-iter b n 1)
```

**Evaluation:**
```
(expt 2 5)

(expt-iter 2 5 1)

(expt-iter 2 4 2)

(expt-iter 2 3 4)

(expt-iter 2 2 8)

(expt-iter 2 1 16)

(expt-iter 2 0 32)
```
**complexities:**
```
time = Theta(n)
space = Theta(1)
```

This is linear iterative process. This version require *Theta(n)* steps and *constant* space. Can we still imporve our solution?

---

**Defination:**
```
b^n = (b^n/2)^2         if n is even
b^n = b * b^(n-1)       if n is odd
```

**Method3:** Recusive process

**Scheme procedures:**

```
(define (expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (expt b (/ n 2))))
        (else (* b (expt b (- n 1))))))
```

The process evolved by this version grows logarithmically with `n` in both space and number of steps. To see this, observe that computing `b^2n`  requires only one more multiplication than computing `b^n`.

**complexities:**
```
time = Theta(log n)
space = Theta(log n)
```

Can we still improve the solution?

**Method4:** Iterative process

**Scheme procedures:**
```
(define (expt-iter b n result)
  (cond ((= n 0) result)
        ((even? n) (expt-iter (square b) (/ n 2) result))
        (else (expt-iter b (- n 1) (* b result)))))

(define (expt b n)
  (expt-iter b n 1)
```

**complexities:**
```
time = Theta(log n)
space = Theta(1)
```

*NOTE:* Order *log-n* become very efficient than order *n* as size of input become very large.

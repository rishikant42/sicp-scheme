# Square roots by Newton's method

### Defination

Square-root of x can be defined as

`x^1/2 = y such that y >= 0 and y^2 = x`

This describes a mathematical function. We could use it to recognize whether one number is the square root of another but it didn't tells us anything about how to actually find the square root of a given number.

---

### How to find the square roots?

The most common way is to use **Newton’s method of successive approximations**, which says that whenever we have a guess `y` for the value of the square root of a number `x`, we can perform a simple manipulation to get a better guess (one closer to the actual square root) by averaging `y` with `x/y`.

Lets try to calculate the square roots of 2. Assume that the start guess value is 1

| Guess            | Quotient           | Average                      |
| -----------------|:------------------:| ---------------------------: |
| 1                | (2/1)              | ((2 + 1)/2) = 1.5            |
| 1.5              | (2/1.5) = 1.3333   | ((1.3333 + 1.5)/2) = 1.4167  |
| 1.4167           |(2/1.4167) = 1.4118 |((1.4167 + 1.4118)/2) = 1.4142|
|1.4142            | .......            | ....                         |

---

### Scheme's procedure

1 Convert Above square root defination in procedure

```
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x)
               x)))
```

2 Define `good-enough` and `imporve` procedures. We call guess is good-enough if the absolute difference in square-of-guess and actual number is less than 0.001.

```
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
  
(define (improve guess x)
  (average guess (/ x guess)))
```

3 Define `square` and `average` procedure.

```
(define (square x) (* x x))

(define (average x y) (/ (+ x y) 2))
```

3 Start the approximation by assuming initial guess value is 1

```
(define (sqrt x)
  (sqrt-iter 1.0 x))
```

### Test
```
1 ]=> (sqrt 2)

;Value: 1.4142156862745097

1 ]=> (sqrt 9)

;Value: 3.00009155413138

1 ]=> (sqrt 0.5)

;Value: .7071078431372548
```

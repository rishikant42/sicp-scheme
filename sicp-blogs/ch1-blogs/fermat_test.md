### Prime Number:
A prime number is a whole number greater than 1 whose only factors are 1 and
itself.

Example: 2, 3, 5, 7, 11....

How to check a given number is prime or not?

## Method1:
One way to test if a number is prime is to find the number’s divisors. If the
smallest divisor is equal to itself, then the number is prime.

## scheme procedure

```
(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (if (< n 2)
    false
    (= (smallest-divisor n) n)))
```

NOTE: *find-divisor* procedure is based on the fact that if n is not prime it
must have a divisor less than or equal to *n^1/2*. This means that the
algorithm need only test divisors between 1 and n^1/2. Consequently, the number
of steps required to identify n as prime will have order of growth
*theta(n^1/2)*.

## Method2: Fermat’s Little Theorem

If *n* is a prime number and *a* is any positive integer less than *n*, then a
raised to the nth power is congruent to a modulo *n*. Two numbers are said to
be congruent modulo n if they both have the same remainder when divided by n.

Example: n = 5,  a = 4  ==> 4^5 := 4 mod 5 ==> 1024 := 4 mod 5

"1024 mod 5 = 4 mod 5, Hence 1024 and 4 are congruent modulo 5."

If n is not prime, then, in general, most of the numbers a< n will not satisfy
the above relation. This leads to the following algorithm for testing
primality: Given a number n, pick a random number a < n and compute the
remainder of a n modulo n. If the result is not equal to a, then n is certainly
not prime. If it is a, then chances are good that n is prime. Now pick another
random number a and test it with the same method. If it also satisfies the
equation, then we can be even more confident that n is prime. By trying more
and more values of a, we can increase our confidence in the result. This
algorithm is known as the Fermat test.

## Scheme procedure
```
;; b^n mod m
(define (expmod a n m)
  (remainder (expt a n) m))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
```

The above procedure runs the test a given number of times, as specified by a
parameter. Its value is true if the test succeeds every time, and false
otherwise.

In above method, the number of steps grows logarithmically with *n* since
*expt* procedure have O(log n) running time.

**Probabilistic algorithms**

The Fermat test differs in character from most familiar algorithms, in which
one computes an answer that is guaranteed to be correct. Here, the answer
obtained is only probably correct. More precisely, if n ever fails the Fermat
test, we can be certain that n is not prime. But the fact that n passes the
test, while an extremely strong indication, is still not a guarantee that n is
prime. What we would like to say is that for any number n, if we perform the
test enough times and find that n always passes the test, then the probability
of error in our primality test can be made as small as we like.

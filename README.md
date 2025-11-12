# functional-programming-lab1

Григорьев Давид Владимирович Р3315 389491

вариант 9, 21

# Special Pythagorean Triplet

## Problem 9

A Pythagorean triplet is a set of three natural numbers, $a < b < c$, for which,

$$
a^2 + b^2 = c^2.
$$

For example, $3^2 + 4^2 = 9 + 16 = 25 = 5^2$.

There exists exactly one Pythagorean triplet for which $a + b + c = 1000$.
Find the product $abc$.

## result
```
-- tailRecursion --
Pythagorean triplet: 200 375 425
Product: 31875000
Execution Time: 0.201 ms
-- specialSyntax --
Pythagorean triplet: 200 375 425
Product: 31875000
Execution Time: 0.895 ms
-- recusion --
Pythagorean triplet: 200 375 425
Product: 31875000
Execution Time: 0.637 ms
-- modules --
Pythagorean triplet: 200 375 425
Product: 31875000
Execution Time: 0.836 ms
```

# Amicable Numbers

## Problem 21

Let $d(n)$ be defined as the sum of proper divisors of $n$ (numbers less than $n$ which divide evenly into $n$).

If $d(a) = b$ and $d(b) = a$, where $a \ne b$, then $a$ and $b$ are an amicable pair and each of $a$ and $b$ are called amicable numbers.

For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore $d(220) = 284$. The proper divisors of 284 are 1, 2, 4, 71 and 142; so $d(284) = 220$.

Evaluate the sum of all the amicable numbers under 10000.

## result
```
-- modules --Sum of all the amicable numbers under 10000 is 31626
Execution Time: 0.515 ms
-- recursion --Sum of all the amicable numbers under 10000 is 31626
Execution Time: 0.834 ms
```

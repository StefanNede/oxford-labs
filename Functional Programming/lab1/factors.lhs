Practical 1: Factoring Numbers

Here is a simple method for finding the smallest prime factor of a positive
integer:

> factor :: Integer -> (Integer, Integer)
> factor n = factorFrom 2 n

> factorFrom :: Integer -> Integer -> (Integer, Integer)
> factorFrom m n | r == 0    = (m,q)
>                | otherwise = factorFrom (m+1) n
>    where (q,r) = n `divMod` m

for example

*Main> factor 7654321
(19,402859)

because 

*Main> 19 * 402859
7654321

Repeatedly extracting the smallest factor will return a list
of prime factors:

> factors :: Integer -> [Integer]
> factors n = factorsFrom 2 n

> factorsFrom :: Integer -> Integer -> [Integer]
> factorsFrom m n | n == 1    = []
>                 | otherwise = p:factorsFrom p q
>    where (p,q) = factorFrom m n

for example

*Main> factor 123456789
(3,41152263)
*Main> factors 123456789
[3,3,3607,3803]



SOLUTIONS
Exercise 1:
factor 0 has value (2,0)
factor 1 won't return a value and will be stuck in an infinite 'loop' because r (the remainder) will remain as 1 for all values of m 
	
Exercise 2:
Running the code has given me what I predicted. I had to interrupt 'factor 1' by typing control-c as it was stuck in an infinite loop as I predicted.
	
Exercise 3:
The smallest factor of n cannot be both bigger than root n and less than n because then the factor pair multiplied together will be greater than n i.e. if the factors are a and b and they're both bigger than root n ab > n which is not possible - one of a or b must be less than root n.

> factor1 :: Integer -> (Integer, Integer)
> factor1 n = factorFrom1 2 n
> factorFrom1 :: Integer -> Integer -> (Integer, Integer)
> factorFrom1 m n | r == 0	= (m,q)
>		  | n <= m*m	= (n,1)
>                 | otherwise	= factorFrom1 (m+1) n
>    where (q,r) = n `divMod` m

The order of the guarded equations does matter because it goes through them sequentially in order much the same as how the order of if and else-if statements matter.
The worst case scenario is when n is a prime number where root n recursive calls are required. 

Exercise 4:
q = floor(n/m) 
if q < m then:
floor(n/m) < m
=> n/m <= m 
=> n <= m*m
so q < m is the same as checking if n <= m*m
Testing if 'q<m' rather than 'n<=m*m' is more efficient because less operations need to be carried out as q and m are already calculated whereas m*m needs to be calculated in the latter check.

> factor2 :: Integer -> (Integer, Integer)
> factor2 n = factorFrom2 2 n
> factorFrom2 :: Integer -> Integer -> (Integer, Integer)
> factorFrom2 m n | r == 0	= (m,q)
>		  | q < m 	= (n,1)
>                 | otherwise	= factorFrom2 (m+1) n
>    where (q,r) = n `divMod` m

Exercise 5:

> factor3 :: Integer -> (Integer, Integer)
> factor3 n = factorFrom3 2 n
> factorFrom3 :: Integer -> Integer -> (Integer, Integer)
> factorFrom3 m n | r == 0	= (m,q)
>		  | q < m 	= (n,1)
>		  | m == 2 	= factorFrom3 3 n
>                 | otherwise	= factorFrom3 (m+2) n
>    where (q,r) = n `divMod` m

If n is even then (m,q) will be returned instantly as r == 0 and then we want to only use odd numbers as trials so I have added the (m==2) which then passes m as 3 and then I increment m by 2 each time so only odd numbers are used in the future.
I expect this version to be around twice as efficient as we increment m by 2 each time instead of 1 (so half the numbers are technically used as trial divisors). The time complexity will remain the same however.

Exercise 6:
Test results:
ghci> factor3 4830418031570237591
(61,79187180845413731)
(0.00 secs, 91,472 bytes)

ghci> factor3 48294820394809214803924803924809214809
(13,3714986184216093446455754148062247293)
(0.01 secs, 96,960 bytes)

ghci> factor3 8341217
(1,8341217)
(0.01 secs, 739,264 bytes)

2 large primes multiplied together:
ghci> factor3 4171689379031
(500167,8340593)
(0.20 secs, 116,111,496 bytes)

Exercise 7:

> factor4 :: Integer -> (Integer, Integer)
> factor4 n = factorFrom4 2 n 2
> factorFrom4 :: Integer -> Integer -> Integer -> (Integer, Integer)
> factorFrom4 m n s | r == 0	= (m,q)
>		    | q < m 	= (n,1)
> 		    | m == 2 	= factorFrom4 3 n 2
> 		    | m == 3 	= factorFrom4 5 n 2
>                   | otherwise	= factorFrom4 (m+s) n (6-s)
>    where (q,r) = n `divMod` m

Testing:
ghci> factor4 48294820394809214803924803924809214809
(13,3714986184216093446455754148062247293)
(0.01 secs, 96,912 bytes)

ghci> factor4 4171689379031
(500167,8340593)
(0.18 secs, 98,772,760 bytes)

Both are faster or same speed compared to factor3 (testing for factor3 can be seen in exercise 6).

Exercise 8:
There is no very efficient way of getting a list of prime numbers to test with and there is no sequential pattern to generate the next prime number, therefore this would make the process overall less efficient.

PRIME FACTORISATION

Exercise 9:
Testing the factors function:
ghci> factors 68
[2,2,17]
(0.00 secs, 74,224 bytes)
ghci> factors 14569
[17,857]
(0.01 secs, 375,952 bytes)
ghci> factors 100
[2,2,5,5]
(0.00 secs, 71,336 bytes)

Rewriting factors function:

> factors2 :: Integer -> [Integer]
> factors2 n = factorsFrom2 2 n

> factorsFrom2 :: Integer -> Integer -> [Integer]
> factorsFrom2 m n | n == 1    = []
>                 | otherwise = p:factorsFrom2 p q
>    where (p,q) = factorFrom4 m n (if ((mod (m-5) 6) == 0) then 2 else 4)

Checking that factors and factors2 give the same result returned True :)
ghci> map factors [2..1000] == map factors2 [2..1000]
True
(0.14 secs, 100,866,040 bytes)

Exercise 10:

ghci> factors 221760
[2,2,2,2,2,2,3,3,5,7,11]
(0.01 secs, 89,600 bytes)
ghci> factors2 221760
[2,2,2,2,2,2,3,3,5,7,11]
(0.01 secs, 89,840 bytes)


ghci> factors 8616460799
[89681,96079]
(0.06 secs, 34,659,672 bytes)
ghci> factors2 8616460799
[89681,96079]
(0.05 secs, 34,659,672 bytes)

It takes 0.05 seconds to find the 2 factors of Jevons' number -> 89681, 96079

ghci> factors 4382094863
[167,26240089]
(11.44 secs, 9,446,504,032 bytes)
ghci> factors2 4382094863
[167,26240089]
(11.45 secs, 9,446,504,032 bytes)

It seems that the 2 factors functions are very similar in terms of performance.

OPTIONAL EXERCISES

As n is an odd number and n = u x v, u and v are also both odd numbers. Therefore (u+v) and (v-u) are both even numbers as the difference between 2 odd numbers is even. Therefore, dividing both (u+v) and (v-u) by 2 will still result in a whole number as all even numbers have 2 as a factor.

Exercise 11:
If r<0, then p^2 - q^2 - n must be less than 0 too, so n > (p+q)(p-q) so we need to increase the value of (p+q)(p-q). For this to happen we should increase p (by 1).
If r>0, then n < (p+q)(p-q) so we need to decrease the value of (p+q)(p-q) so we should increase q (by 1).

This method is guaranteed to terminate for all odd n because, via the introduction of the optional exercises, we have seen that all odd numbers can be written as the difference of 2 whole numbers squared (x^2 - y^2) and x >= p and y >= p and as we are only increasing p or q when r<0 or r>0 we will eventually reach x and y. When x is reached and y >= p r>0 so we will continue to increase q by 1 until we reach y and vice versa when y is reached first. 

> search :: Integer -> Integer -> Integer -> (Integer, Integer)
> search p q n | r == 0 	= (p+q, p-q)
> 	       | r < 0 		= search (p+1) q n
> 	       | r > 0 		= search p (q+1) n
> 	where r = (p^2)-(q^2)-n

> fermat :: Integer -> (Integer, Integer)
> fermat n = search (isqrt n) 1 n


ghci> fermat 21
(7,3)
(0.01 secs, 75,920 bytes)

ghci> fermat 4382942101
(3902887,1123)
(4.64 secs, 7,445,686,992 bytes)


Exercise 12:
The smallest possible value of x is when y=0. In that case x^2 = n so x = (square root of n). We need to use the floor of the square root of n because n might not be a square number in which case the square root of n is not an Integer but we need it to be, so round down to nearest integer.

> isqrt :: Integer -> Integer
> isqrt = truncate . sqrt . fromInteger

fermat n modified above

ghci> fermat 4382942101
(3902887,1123)
(4.55 secs, 7,322,287,432 bytes)

As we can see fermat is now faster.

Exercise 13:
ghci> fermat 8616460799
(96079,89681)
(0.01 secs, 6,369,384 bytes)

ghci> fermat 1963272347809
(8123471,241679)
(8.10 secs, 12,858,692,920 bytes)
ghci> 

Exercise 14:

> search2 :: Integer -> Integer -> Integer -> Integer -> (Integer, Integer)
> search2 p q n r | r==0	= (p+q,p-q)
> 		  | r<0		= search2 (p+1) q n (r+2*p+1)
> 		  | r>0 	= search2 p (q+1) n (r-2*q-1)

> fermat2 :: Integer -> (Integer, Integer)
> fermat2 n = search2 p 1 n r
> 	where p = isqrt n
> 	      r = p^2 - 1 - n

I declare p in a where statement so that isqrt is only called once instead of it being called 3 times otherwise.

ghci> fermat2 1963272347809
(8123471,241679)
(4.88 secs, 3,339,912,856 bytes)

This is nearly twice as fast as fermat.

Exercise 15:

> isqrt2 :: Integer -> Integer
> isqrt2 n = getRes 1 n

> getRes :: Integer -> Integer -> Integer
> getRes k n | ((k+1)^2) > n	= k
> 	     | otherwise 	= getRes (k+1) n

ghci> map isqrt2 [1..1000] == map isqrt [1..1000]
True
(0.02 secs, 21,182,672 bytes)

So safe to say isqrt2 works correctly.

We can tell which of the 2 inequalities we're dealing with by doing m^2 <= n because if m^2 <= n then it is the inequality m <= sqrt(n) < r, and otherwise it is l <= sqrt(n) < m.
Also m^2 <= n iff m <= sqrt(n) because in both directions m and n are positive so you can square or square root both sides of the inequality without having to change the direction of the inequality.

If l < r and l+1 != r then (l+r)'div'2 >= l+1 with the equality in the case that r = l+2 or r = l+3. Hence (l+r)'div'2 > l.
By symmetry (l+r)'div'2 <= r-1 with equality in the case that r = l+2 or r = l+3. Hence, (l+r)'div'2 < r.
Therefore, inequality is true.


Exercise 16:

> split :: (Integer, Integer) -> Integer -> (Integer, Integer)
> split (l,r) n | m^2 <= n 	 = (m,r)
>		| otherwise	 = (l,m)
>	where m = (l+r) `div` 2

Exercise 17:

> isqrt3 ::  Integer -> Integer
> isqrt3 n = sqrtHelper (1,n) n

> sqrtHelper :: (Integer, Integer) -> Integer -> Integer
> sqrtHelper (l,r) n | (l+1)==r 	= l
> 		     | otherwise	= sqrtHelper (split (l,r) n) n


Exercise 18:

> findUpperBound :: Integer -> Integer -> Integer
> findUpperBound n b | b^2 > n 	= b
> 		     | otherwise 	= findUpperBound n (b*2)

> isqrt4 :: Integer -> Integer
> isqrt4 n = sqrtHelper (1, (findUpperBound n 1)) n

This implementation of isqrt takes approximately log2n (log base 2) steps (time complexity of binary search).
Therefore, it is worth the extra effort.

isqrt4 (2^30000) returns in 0.29 seconds whilst isqrt doesn't return anything for a long time.
(0.29 secs, 221,626,864 bytes) <- isqrt4 running time


module Problems31_41 where

import Data.List (group)

-- Problem 31
-- Determine whether a given integer number is prime. 
factors :: Int -> [Int]
factors n = [ x | x <- [1..n], y <- [1..n], x * y == n ]

nontrivialFactors :: Int -> [Int]
nontrivialFactors n = filter (\x -> x /= 1 && x/= n) (factors n)

isPrime :: Int -> Bool
isPrime n = nontrivialFactors n == []

euclidGCF :: Int -> Int -> Int
euclidGCF a b = if a == b then a else (euclidGCF (abs (a-b)) (min a b))

extendedEuclidGCF :: Int -> Int -> Int
extendedEuclidGCF a b = extendedEuclidGCFHelper a b 1 0 0 1
    where extendedEuclidGCFHelper old_remainder remainder old_s s old_t t
            | remainder /= 0 = let quotient = (old_remainder `div` remainder)
                               in extendedEuclidGCFHelper remainder (old_remainder - quotient * remainder) s (old_s - quotient * s) t (old_t - quotient * t)
            | otherwise      = abs old_remainder

-- Problem 33
-- Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1. 
coprime :: Int -> Int -> Bool
coprime a b = extendedEuclidGCF a b == 1

-- Problem 34
-- Calculate Euler's totient function phi(m).
-- Euler's so-called totient function phi(m) is defined as
-- the number of positive integers r (1 <= r < m) that are coprime to m. 
-- Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1. 
totient :: Int -> Int
totient a = length [ b | b <- [1..a], coprime a b ]

-- Problem 35
-- Determine the prime factors of a given positive integer.
-- Construct a flat list containing the prime factors in ascending order.
-- This is probably really inefficient, I know.
primeFactors :: Int -> [Int]
primeFactors n
    | isPrime n = [n]
    | otherwise = let next_prime = head (nontrivialFactors n)
                  in next_prime : primeFactors (n `div` next_prime)
  
-- Problem 36
-- Determine the prime factors of a given positive integer.
-- Construct a list containing the prime factors and their multiplicity.
prime_factors_mult :: Int -> [(Int, Int)]
prime_factors_mult n = let groups = group $ primeFactors n
                       in zip (map head groups) (map length groups)

-- Problem 37
-- Calculate Euler's totient function phi(m) (improved).
-- See problem 34 for the definition of Euler's totient function.
-- If the list of the prime factors of a number m is known in the form of problem 36
-- then the function phi(m) can be efficiently calculated as follows:
-- Let ((p1 m1) (p2 m2) (p3 m3) ...) be the list of prime factors (and their multiplicities) of a given number m.
-- Then phi(m) can be calculated with the following formula: 
-- phi(m) = (p1 - 1) * p1 ** (m1 - 1) * 
--          (p2 - 1) * p2 ** (m2 - 1) * 
--          (p3 - 1) * p3 ** (m3 - 1) * ...
improvedTotient :: Int -> Int
improvedTotient 1 = 1
improvedTotient a = product $ map phiStep (prime_factors_mult a)
    where phiStep (p, m) = (p - 1) * p ^ (m - 1)

-- Problem 38 
-- Compare the two methods of calculating Euler's totient function.
-- Use the solutions of problems 34 and 37 to compare the algorithms.
-- Take the number of reductions as a measure for efficiency.
-- Try to calculate phi(10090) as an example.
-- I have noted that for 100000 (an order of magnitude greater than the test number suggested),
-- improvedTotient is much faster than totient. 10090 is faster with regular totient somehow.

-- Problem 39
-- A list of prime numbers.
-- Given a range of integers by its lower and upper limit,
-- construct a list of all prime numbers in that range. 
primesR :: Int -> Int -> [Int]
primesR a b = filter isPrime [a..b]

-- Problem 40
-- Goldbach's conjecture.
-- Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers.
-- Example: 28 = 5 + 23. It is one of the most famous facts in number theory
-- that has not been proved to be correct in the general case.
-- It has been numerically confirmed up to very large numbers
-- (much larger than we can go with our Prolog system).
-- Write a predicate to find the two prime numbers that sum up to a given even integer. 
goldbach :: Int -> (Int, Int)
goldbach n = head [ (a, b) | a <- primesR 2 n, b <- primesR 2 n, a + b == n ]

-- Problem 41
-- Given a range of integers by its lower and upper limit,
-- print a list of all even numbers and their Goldbach composition.
-- In most cases, if an even number is written as the sum of two prime numbers,
-- one of them is very small. Very rarely, the primes are both bigger than say 50.
-- Try to find out how many such cases there are in the range 2..3000. 
goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList a b = map goldbach (filter even [a..b])

-- goldbachList with a prime lower bound
goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList' a b prime_bound = filter (\(x,y) -> x > prime_bound && y > prime_bound) (goldbachList a b)


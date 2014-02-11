module Problems where

import Data.List
import System.Random (newStdGen, randomRs)
import Control.Monad (guard)
import Data.Function (on)

-- Problem 1 
-- Find the last element of a list. 
-- http://www.haskell.org/haskellwiki/99_questions/1_to_10#Problem_1
myLast :: [a] -> a
myLast = last

-- Problem 2
-- Find the last but one element of a list.
-- http://www.haskell.org/haskellwiki/99_questions/1_to_10#Problem_2
myButLast :: [a] -> a
myButLast = last . init

-- Problem 3
-- Find the K'th element of a list. The first element in the list is number 1. 
-- http://www.haskell.org/haskellwiki/99_questions/1_to_10#Problem_3
elementAt :: [a] -> Int -> a
elementAt = \x y -> x !! (y-1)

-- Problem 4
-- Find the number of elements of a list. 
-- http://www.haskell.org/haskellwiki/99_questions/1_to_10#Problem_4
myLength :: [a] -> Int
myLength = length

-- Problem 5
-- Reverse a list. 
-- http://www.haskell.org/haskellwiki/99_questions/1_to_10#Problem_5
myReverse :: [a] -> [a]
myReverse = reverse

-- Problem 6
-- Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x). 
-- http://www.haskell.org/haskellwiki/99_questions/1_to_10#Problem_6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome word = word == (reverse word)

-- Problem 7
-- Flatten a nested list structure.
-- Transform a list, possibly holding lists as elements into a `flat' list
-- by replacing each list with its elements (recursively). 
-- http://www.haskell.org/haskellwiki/99_questions/1_to_10#Problem_7
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem elt) = [elt]
flatten (List []) = []
flatten (List list) = concatMap flatten list

-- Problem 8
-- Eliminate consecutive duplicates of list elements.
-- If a list contains repeated elements they should be replaced with a single copy of the element.
-- The order of the elements should not be changed.
-- http://www.haskell.org/haskellwiki/99_questions/1_to_10#Problem_8
-- NOTE: I could only get this working with Strings, not lists in general.
compress :: String -> String
compress = foldl (\x y -> if length x == 0 || (last x) /= y then x++[y] else x) ""
-- To get it working with polymorphic lists, maybe I have to do something like this:
-- compress :: (Monoid m, Eq m) => [m] -> [m]
-- compress = foldl (\x y -> if length x == 0 || (last x) /= y then (mappend x [y]) else x) mempty

-- Problem 9
-- Pack consecutive duplicates of list elements into sublists.
-- If a list contains repeated elements they should be placed in separate sublists. 
-- http://www.haskell.org/haskellwiki/99_questions/1_to_10#Problem_9
pack :: (Eq a) => [a] -> [[a]]
pack = group

-- Problem 10
-- Run-length encoding of a list. Use the result of problem P09
-- to implement the so-called run-length encoding data compression method.
-- Consecutive duplicates of elements are encoded as lists (N E)
-- where N is the number of duplicates of the element E. 
-- http://www.haskell.org/haskellwiki/99_questions/1_to_10#Problem_10
encode :: (Eq a) => [a] -> [(Int, a)]
encode list = let groups = pack list in zip (map length groups) (map head groups)

-- Problem 11
-- Modified run-length encoding.
-- Modify the result of problem 10 in such a way that
-- if an element has no duplicates it is simply copied into the result list.
-- Only elements with duplicates are transferred as (N E) lists. 
data Encoding a = Multiple Int a | Single a deriving (Show, Eq)

encodeModified :: (Eq a) => [a] -> [Encoding a]
encodeModified list = map toEncoding (pack list)
    where toEncoding = \x -> if (length x) == 1 then Single (head x) else Multiple (length x) (head x)

-- Problem 12
-- Decode a run-length encoded list.
-- Given a run-length code list generated as specified in problem 11. Construct its uncompressed version. 
decodeModified :: (Eq a) => [Encoding a] -> [a]
decodeModified list = concatMap decode list
    where decode (Single a) = [a]
          decode (Multiple n a) = take n (cycle [a])

-- Problem 13
-- Run-length encoding of a list (direct solution).
-- Implement the so-called run-length encoding data compression method directly.
-- I.e. don't explicitly create the sublists containing the duplicates, as in problem 9,
-- but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.
-- NOTE: Not sure I understand the difference between problems 13 and 11. I think I already did this in problem 11.

-- Problem 14
-- Duplicate the elements of a list. 
dupli :: [a] -> [a]
dupli = concatMap (\x -> [x]++[x])

-- Problem 15
-- Replicate the elements of a list a given number of times. 
repli :: [a] -> Int -> [a]
repli list n = concatMap (\x -> take n (cycle [x])) list

-- Problem 16
-- Drop every N'th element from a list. 
-- Ugly, I know.
dropEvery :: [a] -> Int -> [a]
dropEvery list n = concatMap (\(x,_) -> x) filteredList
    where filteredList = filter (\x -> (snd x) `mod` n /= 0 && (snd x) /= 0) listWithIndices
          listWithIndices = foldl (\x y -> x++[([y],1+snd(last x))]) [([],0)] list

-- Problem 17
-- Split a list into two parts; the length of the first part is given.
-- Do not use any predefined predicates.
split :: [a] -> Int -> ([a], [a])
split list n = (first_n, rest)
    where first_n = take n list
          rest = reverse (take ((length list)-n) (reverse list))

-- Problem 18
-- Extract a slice from a list.
-- Given two indices, i and k,
-- the slice is the list containing the elements between the i'th and k'th element of the original list
-- (both limits included). Start counting the elements with 1. 
slice :: [a] -> Int -> Int -> [a]
slice list i k = concatMap (\(x,_) -> x) filteredList
    where filteredList = filter withinRange listWithIndices
          withinRange = (\x -> (snd x) >= i && (snd x) <= k)
          listWithIndices = foldl (\x y -> x++[([y],1+snd(last x))]) [([],0)] list

-- Problem 19
-- Rotate a list N places to the left.
-- Hint: Use the predefined functions length and (++). 
rotate :: [a] -> Int -> [a]
rotate list n
    | n >= 0    = (drop n list) ++ (take n list)
    | otherwise = let dropAmt = (length list) + n
                  in drop dropAmt list ++ take dropAmt list

-- Problem 20
-- Remove the K'th element from a list. 
removeAt :: Int -> [a] -> (a, [a])
removeAt n list = (list !! (n-1), take (n-1) list ++ drop n list)

-- Problem 21
-- Insert an element at a given position into a list. 
insertAt :: a -> [a] -> Int -> [a]
insertAt x list 1 = x:list
insertAt x list i = (head list):(insertAt x (tail list) (i-1))

-- Problem 22
-- Create a list containing all integers within a given range. 
range :: Int -> Int -> [Int]
range i k = [i..k]

-- Problem 23
-- Extract a given number of randomly selected elements from a list.
rnd_select :: (Show a) => [a] -> Int -> IO ()
rnd_select list n = do
  g <- newStdGen
  print $ map (list !!) $ take n (randomRs (0, (length list - 1)) g)

-- Problem 24
-- Lotto: Draw N different random numbers from the set 1..M. 
diff_select :: Int -> Int -> IO ()
diff_select n max_num = rnd_select [1..max_num] n

-- Problem 25
-- Generate a random permutation of the elements of a list. 
rnd_permu :: (Show a) => [a] -> IO ()
rnd_permu list = rnd_select list (length list)

-- Problem 26
-- Generate the combinations of K distinct objects chosen from the N elements of a list
-- In how many ways can a committee of 3 be chosen from a group of 12 people?
-- We all know that there are C(12,3) = 220 possibilities
-- (C(N,K) denotes the well-known binomial coefficients).
-- For pure mathematicians, this result may be great.
-- But we want to really generate all the possibilities in a list.
-- combinations 3 "abcdef" ~?= ["abc","abd","abe",...]
combinations k list = filter (\x -> length x == k) (subsequences list)


-- Problem 27
-- Group the elements of a set into disjoint subsets.
-- a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons?
-- Write a function that generates all the possibilities and returns them in a list.
-- Example:
-- P27> group [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
-- [[["aldo","beat"],["carla","david","evi"],["flip","gary","hugo","ida"]],...]
-- (altogether 1260 solutions) 
--
-- b) Generalize the above predicate in a way that we can specify a list of group sizes
-- and the predicate will return a list of groups.
-- Example:
-- 27> group [2,2,5] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
-- [[["aldo","beat"],["carla","david"],["evi","flip","gary","hugo","ida"]],...]
-- (altogether 756 solutions)
--
-- Note that we do not want permutations of the group members; i.e. ((ALDO BEAT) ...)
-- is the same solution as ((BEAT ALDO) ...).
-- However, we make a difference between ((ALDO BEAT) (CARLA DAVID) ...)
-- and ((CARLA DAVID) (ALDO BEAT) ...).
-- You may find more about this combinatorial problem in a good book on discrete mathematics
-- under the term "multinomial coefficients". 

-- http://stackoverflow.com/a/15319164/2954849
areEqual :: (Ord a) => [a] -> [a] -> Bool
areEqual a b = sort a == sort b

areDeeplyEqual :: (Ord a) => [[a]] -> [[a]] -> Bool
areDeeplyEqual a b = map sort a == map sort b

-- Part a:
disjoint_sets :: (Ord b) => [Int] -> [b] -> [[[b]]]
disjoint_sets [2,3,4] list = do
   a <- combinations 2 list
   b <- combinations 3 list
   c <- combinations 4 list
   guard (areEqual (a++b++c) list)
   return (nub [a]++[b]++[c])

-- Part b:
disjoint_sets partitionSizes list = nubBy areDeeplyEqual $ filter validPartition $ nub $ combs
    where validPartition x = (matchesList x) && (matchesSizes x)
          matchesList x = areEqual list (concat x)
          matchesSizes x = map length x == partitionSizes
          combs = concatMap subsequences $ map subsequences $ permutations list

-- Problem 28
-- Sorting a list of lists according to length of sublists
-- a) We suppose that a list contains elements that are lists themselves.
-- The objective is to sort the elements of this list according to their length.
-- E.g. short lists first, longer lists later, or vice versa.
-- Example:
-- Prelude>lsort ["abc","de","fgh","de","ijkl","mn","o"]
-- Prelude>["o","de","de","mn","abc","fgh","ijkl"]
--
-- b) Again, we suppose that a list contains elements that are lists themselves.
-- But this time the objective is to sort the elements of this list according to their length frequency;
-- i.e., in the default, where sorting is done ascendingly,
-- lists with rare lengths are placed first, others with a more frequent length come later. 

-- Part a:
lsort :: (Ord a) => [[a]] -> [[a]]
lsort = sortBy ((compare) `on` length)

-- Part b:
lfsort list = sortBy frequencySort list
    where frequencySort = (compare) `on` frequency
          frequency = (\x -> length (filter (\y -> length y == length x) list))

-- Problem 31
-- Determine whether a given integer number is prime. 
factors :: Int -> [Int]
factors n = [ x | x <- [1..n], y <- [1..n], x * y == n ]

nonPrimeFactors :: Int -> [Int]
nonPrimeFactors n = filter (\x -> x /= 1 && x/= n) (factors n)

isPrime :: Int -> Bool
isPrime n = nonPrimeFactors n == []

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
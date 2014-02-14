module Problems21_28 where

import Data.List (nub, nubBy, subsequences, permutations, sort, sortBy)
import System.Random (newStdGen, randomRs)
import Control.Monad (guard)
import Data.Function (on)

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
combinations :: Int -> [a] -> [[a]]
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
lfsort :: (Ord a) => [[a]] -> [[a]]
lfsort list = sortBy frequencySort list
    where frequencySort = (compare) `on` frequency
          frequency = (\x -> length (filter (\y -> length y == length x) list))


module Problems where

import Data.List

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
--compress :: (Monoid m, Eq m) => [m] -> [m]
--compress = foldl (\x y -> if length x == 0 || (last x) /= y then (mappend x [y]) else x) mempty

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
encode list = let groups = group list in zip (map length groups) (map head groups)
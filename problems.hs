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
dropEvery list n = concatMap (\(x,y) -> x) filteredList
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
-- Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 1. 
slice :: [a] -> Int -> Int -> [a]
slice list i k = concatMap (\(x,y) -> x) filteredList
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
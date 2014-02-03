module Problems where

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
-- Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively). 
-- http://www.haskell.org/haskellwiki/99_questions/1_to_10#Problem_7
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem elt) = [elt]
flatten (List []) = []
flatten (List list) = concatMap flatten list
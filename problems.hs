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
-- Problem 2
-- Find the last but one element of a list.

import Test.HUnit

myButLastAssertion1 :: Assertion
myButLastAssertion1 = myButLast [1,2,3,4] @=? 3

myButLastAssertion2 :: Assertion
myButLastAssertion2 = myButLast ['a'..'z'] @=? 'y'

myButLast :: [a] -> a
myButLast = last . init
-- Problem 1 
-- Find the last element of a list. 

import Test.HUnit

myLastAssertion1 :: Assertion
myLastAssertion1 = myLast [1,2,3,4] @=? 4

myLastAssertion2 :: Assertion
myLastAssertion2 = myLast ['x','y','z'] @=? 'z'

myLast :: [a] -> a
myLast = last

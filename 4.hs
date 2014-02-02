-- Problem 4
-- Find the number of elements of a list. 

import Test.HUnit

myLengthAssertion1 :: Assertion
myLengthAssertion1 = myLength [123, 456, 789] @=? 3

myLength2 :: Assertion
myLength2 = myLength "Hello, world!" @=? 13

myLength :: [a] -> Int
myLength = length
-- Problem 3
-- Find the K'th element of a list. The first element in the list is number 1. 

import Test.HUnit

elementAtAssertion1 :: Assertion
elementAtAssertion1 = elementAt [1,2,3] 2 @=? 2

elementAtAssertion2 :: Assertion
elementAtAssertion2 = elementAt "haskell" 5 @=? 'e'

elementAt :: [a] -> Int -> a
elementAt = (!!)
module Tests11_20 where

import Test.HUnit
import Problems11_20

problem11test1 = encodeModified "aaaabccaadeeee" ~?= [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
problem11tests = TestList [ problem11test1 ]

problem12test1 = decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e'] ~?= "aaaabccaadeeee"
problem12tests = TestList [ problem12test1 ]

problem13tests = problem11tests

problem14test1 = dupli [1, 2, 3] ~?= [1,1,2,2,3,3]
problem14tests = TestList [ problem14test1 ]

problem15test1 =repli "abc" 3 ~=? "aaabbbccc"
problem15tests = TestList [ problem15test1 ]

problem16test1 = dropEvery "abcdefghik" 3 ~=? "abdeghk"
problem16tests = TestList [ problem16test1 ]

problem17test1 = split "abcdefghik" 3 ~?= ("abc", "defghik")
problem17tests = TestList [ problem17test1 ]

problem18test1 = slice ['a','b','c','d','e','f','g','h','i','k'] 3 7 ~?= "cdefg"
problem18tests = TestList [ problem18test1 ]

problem19test1 = rotate ['a','b','c','d','e','f','g','h'] 3 ~?= "defghabc" 
problem19test2 = rotate ['a','b','c','d','e','f','g','h'] (-2) ~?= "ghabcdef"
problem19tests = TestList [ problem19test1, problem19test2 ]

problem20test1 = removeAt 2 "abcd" ~?= ('b',"acd")
problem20tests = TestList [ problem20test1 ]

tests = TestList [ problem11tests, problem12tests, problem13tests, problem14tests, problem15tests
                 , problem16tests, problem17tests, problem18tests, problem19tests, problem20tests ]

main = runTestTT tests

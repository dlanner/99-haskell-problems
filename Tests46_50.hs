module Tests46_50 where

import Test.HUnit
import Problems46_50

problem46test1 :: Test
problem46test1 = and' True True ~?= True
problem46test2 :: Test
problem46test2 = and' True False ~?= False
problem46test3 :: Test
problem46test3 = and' False True ~?= False
problem46test4 :: Test
problem46test4 = and' False False ~?= False

problem46test5 :: Test
problem46test5 = or' True True ~?= True
problem46test6 :: Test
problem46test6 = or' True False ~?= True
problem46test7 :: Test
problem46test7 = or' False True ~?= True
problem46test8 :: Test
problem46test8 = or' False False ~?= False

problem46test9 :: Test
problem46test9 = nand' True True ~?= False
problem46test10 :: Test
problem46test10 = nand' True False ~?= True
problem46test11 :: Test
problem46test11 = nand' False True ~?= True
problem46test12 :: Test
problem46test12 = nand' False False ~?= True

problem46test13 :: Test
problem46test13 = nor' True True ~?= False
problem46test14 :: Test
problem46test14 = nor' True False ~?= False
problem46test15 :: Test
problem46test15 = nor' False True ~?= False
problem46test16 :: Test
problem46test16 = nor' False False  ~?= True

problem46test17 :: Test
problem46test17 = xor' True True ~?= False
problem46test18 :: Test
problem46test18 = xor' True False ~?= True
problem46test19 :: Test
problem46test19 = xor' False True ~?= True
problem46test20 :: Test
problem46test20 = xor' False False  ~?= False

problem46test21 :: Test
problem46test21 = impl' True True ~?= True
problem46test22 :: Test
problem46test22 = impl' True False ~?= False
problem46test23 :: Test
problem46test23 = impl' False True ~?= True
problem46test24 :: Test
problem46test24 = impl' False False  ~?= True

problem46test25 :: Test
problem46test25 = equ' True True ~?= True
problem46test26 :: Test
problem46test26 = equ' True False ~?= False
problem46test27 :: Test
problem46test27 = equ' False True ~?= False
problem46test28 :: Test
problem46test28 = equ' False False  ~?= True

problem46test29 :: Test
problem46test29 = truth_rows (\a b -> (and' a (or' a b))) ~?= [ "True | True | True"
                                                              , "True | False | True"
                                                              , "False | True | False"
                                                              , "False | False | False" ]

problem46tests :: Test
problem46tests = TestList [ problem46test1, problem46test2, problem46test3, problem46test4, problem46test5
                          , problem46test6, problem46test7, problem46test8, problem46test9, problem46test10
                          , problem46test11, problem46test12, problem46test13, problem46test14, problem46test15
                          , problem46test16, problem46test17, problem46test18, problem46test19, problem46test20
                          , problem46test21, problem46test22, problem46test23, problem46test24, problem46test25
                          , problem46test26, problem46test27, problem46test28, problem46test29 ]

problem47tests :: Test
problem47tests = TestList []

problem48tests :: Test
problem48tests = TestList []

problem49tests :: Test
problem49tests = TestList []

problem50tests :: Test
problem50tests = TestList []

tests :: Test
tests = TestList [ problem46tests, problem47tests, problem48tests, problem49tests, problem50tests ]

main :: IO Counts
main = runTestTT tests

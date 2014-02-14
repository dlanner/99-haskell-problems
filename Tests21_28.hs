module Tests21_28 where

import Test.HUnit
import Problems21_28

problem21test1 :: Test
problem21test1 = insertAt 'X' "abcd" 2 ~?= "aXbcd"
problem21tests :: Test
problem21tests = TestList [ problem21test1 ]

problem22test1 :: Test
problem22test1 = range 4 9 ~?= [4,5,6,7,8,9]
problem22tests :: Test
problem22tests = TestList [ problem22test1 ]

-- Don't really know how to test IO yet, but my solutions for problems 23, 24, and 25 work.

problem26test1 :: Test
problem26test1 = combinations 3 "abcdef" ~?= ["abc","abd","acd","bcd","abe","ace","bce","ade","bde","cde","abf","acf","bcf","adf","bdf","cdf","aef","bef","cef","def"]
problem26tests :: Test
problem26tests = TestList [ problem26test1 ]

problem27test1 :: Test
problem27test1 = length (disjoint_sets [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]) ~?= 1260
problem27test2 :: Test
problem27test2 = disjoint_sets [1,1,1] ["aldo","beat","carla"] ~?= [[["aldo"],["beat"],["carla"]],[["beat"],["aldo"],["carla"]],[["carla"],["beat"],["aldo"]],[["beat"],["carla"],["aldo"]],[["carla"],["aldo"],["beat"]],[["aldo"],["carla"],["beat"]]]
problem27test3 :: Test
problem27test3 = disjoint_sets [1,2] ["aldo","beat","carla"] ~?= [[["beat"],["aldo","carla"]],[["aldo"],["beat","carla"]],[["carla"],["beat","aldo"]]]
problem27test4 :: Test
problem27test4 = disjoint_sets [2,1] ["aldo","beat","carla"] ~?= [[["aldo","beat"],["carla"]],[["carla","beat"],["aldo"]],[["carla","aldo"],["beat"]]]
problem27test5 :: Test
problem27test5 = disjoint_sets [3] ["aldo","beat","carla"] ~?= [[["aldo","beat","carla"]]]
problem27tests :: Test
problem27tests = TestList [ problem27test1, problem27test2, problem27test3, problem27test4, problem27test5 ]

problem28test1 :: Test
problem28test1 = lsort ["abc","de","fgh","de","ijkl","mn","o"] ~?= ["o","de","de","mn","abc","fgh","ijkl"]
problem28test2 :: Test
problem28test2 = lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"] ~?= ["ijkl","o","abc","fgh","de","de","mn"]
problem28tests :: Test
problem28tests = TestList [ problem28test1, problem28test2 ]

tests :: Test
tests = TestList [ problem21tests, problem22tests, problem26tests, problem27tests, problem28tests ]

main :: IO Counts
main = runTestTT tests

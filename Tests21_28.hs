module Tests21_28 where

import Test.HUnit
import Problems21_28

problem21test1 = insertAt 'X' "abcd" 2 ~?= "aXbcd"
problem21tests = TestList [ problem21test1 ]

problem22test1 = range 4 9 ~?= [4,5,6,7,8,9]
problem22tests = TestList [ problem22test1 ]

-- Don't really know how to test IO yet, but my solutions for problems 23, 24, and 25 work.
problem23tests = TestList [ ]
problem24tests = TestList [ ]
problem25tests = TestList [ ]

problem26test1 = combinations 3 "abcdef" ~?= ["abc","abd","acd","bcd","abe","ace","bce","ade","bde","cde","abf","acf","bcf","adf","bdf","cdf","aef","bef","cef","def"]
problem26tests = TestList [ problem26test1 ]

problem27test1 = length (disjoint_sets [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]) ~?= 1260
problem27test2 = disjoint_sets [1,1,1] ["aldo","beat","carla"] ~?= [[["aldo"],["beat"],["carla"]],[["beat"],["aldo"],["carla"]],[["carla"],["beat"],["aldo"]],[["beat"],["carla"],["aldo"]],[["carla"],["aldo"],["beat"]],[["aldo"],["carla"],["beat"]]]
problem27test3 = disjoint_sets [1,2] ["aldo","beat","carla"] ~?= [[["beat"],["aldo","carla"]],[["aldo"],["beat","carla"]],[["carla"],["beat","aldo"]]]
problem27test4 = disjoint_sets [2,1] ["aldo","beat","carla"] ~?= [[["aldo","beat"],["carla"]],[["carla","beat"],["aldo"]],[["carla","aldo"],["beat"]]]
problem27test5 = disjoint_sets [3] ["aldo","beat","carla"] ~?= [[["aldo","beat","carla"]]]
problem27tests = TestList [ problem27test1, problem27test2, problem27test3, problem27test4, problem27test5 ]

problem28test1 = lsort ["abc","de","fgh","de","ijkl","mn","o"] ~?= ["o","de","de","mn","abc","fgh","ijkl"]
problem28test2 = lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"] ~?= ["ijkl","o","abc","fgh","de","de","mn"]
problem28tests = TestList [ problem28test1, problem28test2 ]

tests = TestList [ problem21tests, problem22tests, problem23tests, problem24tests, problem25tests
                 , problem26tests, problem27tests, problem28tests ]

main = runTestTT tests

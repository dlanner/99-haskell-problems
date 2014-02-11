import Test.HUnit
import Problems

problem1test1 = myLast [1,2,3,4] ~?= 4
problem1test2 = myLast ['x','y','z'] ~?= 'z'
problem1tests = TestList [ problem1test1, problem1test2 ]

problem2test1 = myButLast [1,2,3,4] ~?= 3
problem2test2 = myButLast ['a'..'z'] ~?= 'y'
problem2tests = TestList [ problem2test1, problem2test2 ]

problem3test1 = elementAt [1,2,3] 2 ~?= 2
problem3test2 = elementAt "haskell" 5 ~?= 'e'
problem3tests = TestList [ problem3test1, problem3test2 ]

problem4test1 = myLength [123, 456, 789] ~?= 3
problem4test2 = myLength "Hello, world!" ~?= 13
problem4tests = TestList [ problem4test1, problem4test2 ]

problem5test1 = myReverse "A man, a plan, a canal, panama!" ~?= "!amanap ,lanac a ,nalp a ,nam A"
problem5test2 = myReverse [1,2,3,4] ~?= [4,3,2,1]
problem5tests = TestList [ problem5test1, problem5test2 ]

problem6test1 = isPalindrome [1,2,3] ~?= False
problem6test2 = isPalindrome "madamimadam" ~?= True
problem6test3 = isPalindrome [1,2,4,8,16,8,4,2,1] ~?= True
problem6tests = TestList [ problem6test1, problem6test2, problem6test3 ]

problem7test1 = flatten (Elem 5) ~?= [5]
problem7test2 = flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) ~?= [1,2,3,4,5]
problem7test3 = flatten (List []::(NestedList Int)) ~?= []
problem7tests = TestList [ problem7test1, problem7test2, problem7test3 ]

problem8test1 = compress "aaaabccaadeeee" ~?= "abcade"
problem8tests = TestList [ problem8test1 ]

problem9test1 = pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'] ~?= ["aaaa","b","cc","aa","d","eeee"]
problem9tests = TestList [ problem9test1 ]

problem10test1 = encode "aaaabccaadeeee" ~?= [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
problem10tests = TestList [ problem10test1 ]

problemtests1_10 = TestList [ problem1tests, problem2tests, problem3tests, problem4tests, problem5tests
	                        , problem6tests, problem7tests, problem8tests, problem9tests, problem10tests ]

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

problemtests11_20 = TestList [ problem11tests, problem12tests, problem13tests, problem14tests, problem15tests
	                           , problem16tests, problem17tests, problem18tests, problem19tests, problem20tests ]

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

problemtests21_28 = TestList [ problem21tests, problem22tests, problem23tests, problem24tests, problem25tests
                             , problem26tests, problem27tests, problem28tests ]

problem31test1 = isPrime 7 ~?= True
problem31tests = TestList [ problem31test1 ]

problem32test1 = euclidGCF 36 63 ~?= 9
problem32test2 = extendedEuclidGCF 36 63 ~?= 9
problem32test3 = extendedEuclidGCF (-3) (-6) ~?= 3
problem32test4 = extendedEuclidGCF (-3) 6 ~?= 3
problem32tests = TestList [ problem32test1, problem32test2, problem32test3, problem32test4 ]

problem33test1 = coprime 35 64 ~?= True
problem33tests = TestList [ problem33test1 ]

problem34test1 = totient 10 ~?= 4
problem34test2 = totient 1 ~?= 1
problem34tests = TestList [ problem34test1, problem34test2 ]

problem35test1 = primeFactors 315 ~?= [3, 3, 5, 7]
problem35tests = TestList [ problem35test1 ]

problem36test1 = prime_factors_mult 315 ~?= [(3,2),(5,1),(7,1)]
problem36tests = TestList [ problem36test1 ]

problem37tests = TestList [ ]
problem38tests = TestList [ ]
problem39tests = TestList [ ]
problem40tests = TestList [ ]
problem41tests = TestList [ ]

problemtests31_41 = TestList [ problem31tests, problem32tests, problem33tests, problem34tests, problem35tests
                             , problem36tests, problem37tests, problem38tests, problem39tests, problem40tests
                             , problem41tests ]

main = runTestTT $ TestList [ problemtests1_10, problemtests11_20, problemtests21_28, problemtests31_41 ]
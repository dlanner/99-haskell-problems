module Tests31_41 where

import Test.HUnit
import Problems31_41

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

problem37test1 = improvedTotient 10 ~?= 4
problem37test2 = improvedTotient 1 ~?= 1
problem37tests = TestList [ problem37test1, problem37test2 ]

-- Problem 38 is not an actual problem

problem39test1 = primesR 10 20 ~?= [11,13,17,19]
problem39tests = TestList [ problem39test1 ]

problem40test1 = goldbach 28 ~?= (5, 23)
problem40tests = TestList [ problem40test1 ]

problem41test1 = goldbachList 9 20 ~?= [(3,7),(5,7),(3,11),(3,13),(5,13),(3,17)]
problem41test2 = goldbachList' 9 20 4 ~?= [(5,7),(5,13)]
problem41tests = TestList [ problem41test1, problem41test2 ]

problemtests31_41 = TestList [ problem31tests, problem32tests, problem33tests, problem34tests, problem35tests
                             , problem36tests, problem37tests, problem39tests, problem40tests, problem41tests ]

main = runTestTT problemtests31_41

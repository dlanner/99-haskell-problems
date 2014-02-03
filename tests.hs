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

main = runTestTT $ TestList
	[ problem1tests, problem2tests, problem3tests, problem4tests, problem5tests
	, problem6tests, problem7tests, problem8tests, problem9tests, problem10tests ]

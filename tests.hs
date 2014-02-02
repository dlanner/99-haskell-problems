import Test.HUnit
import Problems

problem1test1 = myLast [1,2,3,4] ~=? 4
problem1test2 = myLast ['x','y','z'] ~=? 'z'
problem1tests = TestList [ problem1test1, problem1test2 ]

problem2test1 = myButLast [1,2,3,4] ~=? 3
problem2test2 = myButLast ['a'..'z'] ~=? 'y'
problem2tests = TestList [ problem2test1, problem2test2 ]

problem3test1 = elementAt [1,2,3] 2 ~=? 2
problem3test2 = elementAt "haskell" 5 ~=? 'e'
problem3tests = TestList [ problem3test1, problem3test2 ]

problem4test1 = myLength [123, 456, 789] ~=? 3
problem4test2 = myLength "Hello, world!" ~=? 13
problem4tests = TestList [ problem4test1, problem4test2 ]

main = runTestTT $ TestList [ problem1tests, problem2tests, problem3tests, problem4tests ]

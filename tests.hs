import Test.HUnit
import Tests1_10
import Tests11_20
import Tests21_28
import Tests31_41
import Tests46_50

main = runTestTT $ TestList [ problemtests1_10, problemtests11_20, problemtests21_28, problemtests31_41, problemtests46_50 ]

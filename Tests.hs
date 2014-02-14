import Test.HUnit
import qualified Tests1_10 as T1_10
import qualified Tests11_20 as T11_20
import qualified Tests21_28 as T21_28
import qualified Tests31_41 as T31_41
import qualified Tests46_50 as T46_50

main = runTestTT $ TestList [ T1_10.tests, T11_20.tests, T21_28.tests, T31_41.tests, T46_50.tests ]

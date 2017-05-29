import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase, (@=?))


main = defaultMain unitTests

unitTests =
  testGroup
    "Unit tests"
    [test1, test2]

test1 = testCase "something1" $ 1 @=? 1
test2 = testCase "something2" $ 1 @=? 2

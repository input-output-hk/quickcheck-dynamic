module Test.QuickCheck.DynamicLogic.QuantifySpec where

import Test.QuickCheck (Arbitrary (..), Gen, Property)
import Test.QuickCheck.DynamicLogic.Quantify (validQuantification, withGenQ)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

propWithGenQRestrictsValues :: Property
propWithGenQRestrictsValues =
  validQuantification $ withGenQ (arbitrary :: Gen Int) ((< 10) . abs) (shrink @Int)

tests :: TestTree
tests =
  testGroup
    "Quantification"
    [testProperty "withGenQ restricts possible generated values" propWithGenQRestrictsValues]

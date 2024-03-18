{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Spec.DynamicLogic.RegistryModel qualified
import Test.QuickCheck.DynamicLogic.QuantifySpec qualified
import Test.QuickCheck.StateModelSpec qualified
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "dynamic logic"
    [ Spec.DynamicLogic.RegistryModel.tests
    , Test.QuickCheck.DynamicLogic.QuantifySpec.tests
    , Test.QuickCheck.StateModelSpec.tests
    ]

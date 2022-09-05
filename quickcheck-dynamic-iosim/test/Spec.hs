{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Spec.DynamicLogic.RegistryModel qualified
import Test.Tasty
import Test.Tasty.Runners.Reporter qualified as Reporter

main :: IO ()
main = defaultMainWithIngredients [Reporter.ingredient] tests

tests :: TestTree
tests =
  testGroup
    "dynamic logic"
    [ Spec.DynamicLogic.RegistryModel.tests
    ]

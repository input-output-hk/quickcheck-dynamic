{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Spec.DynamicLogic.CounterModel qualified
import Spec.DynamicLogic.RegistryModel qualified
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "dynamic logic"
    [ Spec.DynamicLogic.RegistryModel.tests
    , Spec.DynamicLogic.CounterModel.tests
    ]

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}

module Test.QuickCheck.StateModelSpec where

import Control.Monad.Reader (lift)
import Data.IORef (newIORef)
import Data.List (isInfixOf)
import Spec.DynamicLogic.Counters (Counter (..), FailingCounter, SimpleCounter (..))
import Test.QuickCheck (Property, Result (..), Testable, chatty, choose, counterexample, noShrinking, property, stdArgs)
import Test.QuickCheck.Extras (runPropertyReaderT)
import Test.QuickCheck.Monadic (assert, monadicIO, monitor, pick)
import Test.QuickCheck.StateModel (
  Actions,
  lookUpVarMaybe,
  mkVar,
  runActions,
  underlyingState,
  viewAtType,
  pattern Actions,
 )
import Test.QuickCheck.Test (test, withState)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?))
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "Running actions"
    [ testProperty "simple counter" $ prop_counter
    , testProperty "returns final state updated from actions" prop_returnsFinalState
    , testProperty "environment variables indices are 1-based " prop_variablesIndicesAre1Based
    , testCase "prints distribution of actions and polarity" $ do
        Success{output} <- captureTerminal prop_returnsFinalState
        "100.00% +Inc" `isInfixOf` output @? "Output does not contain '100.00% +Inc'"
        "Action polarity" `isInfixOf` output @? "Output does not contain 'Action polarity'"
    , testCase "prints counterexample as sequence of steps when postcondition fails" $ do
        Failure{output} <- captureTerminal prop_failsOnPostcondition
        "do action $ Inc'" `isInfixOf` output @? "Output does not contain \"do action $ Inc'\": " <> output
    ]

captureTerminal :: Testable p => p -> IO Result
captureTerminal p =
  withState stdArgs{chatty = False} $ \st ->
    test st (property p)

prop_counter :: Actions Counter -> Property
prop_counter as = monadicIO $ do
  ref <- lift $ newIORef (0 :: Int)
  runPropertyReaderT (runActions as) ref
  assert True

prop_returnsFinalState :: Actions SimpleCounter -> Property
prop_returnsFinalState actions@(Actions as) =
  monadicIO $ do
    ref <- lift $ newIORef (0 :: Int)
    (s, _) <- runPropertyReaderT (runActions actions) ref
    assert $ count (underlyingState s) == length as

prop_variablesIndicesAre1Based :: Actions SimpleCounter -> Property
prop_variablesIndicesAre1Based actions@(Actions as) =
  noShrinking $ monadicIO $ do
    ref <- lift $ newIORef (0 :: Int)
    (_, env) <- runPropertyReaderT (runActions actions) ref
    act <- pick $ choose (0, length as - 1)
    monitor $
      counterexample $
        unlines
          [ "Env:  " <> show (viewAtType @Int <$> env)
          , "Actions:  " <> show as
          , "Act:  " <> show act
          ]
    assert $ null as || lookUpVarMaybe env (mkVar $ act + 1) == Just act

prop_failsOnPostcondition :: Actions FailingCounter -> Property
prop_failsOnPostcondition actions =
  monadicIO $ do
    ref <- lift $ newIORef (0 :: Int)
    runPropertyReaderT (runActions actions) ref
    assert True

{-# LANGUAGE TemplateHaskell #-}

module Issue31 where

import Control.Monad.Identity

import Debug.Trace (trace)
import Test.QuickCheck (Property, verboseCheck)
import Test.QuickCheck qualified as QC
import Test.QuickCheck.DynamicLogic
import Test.QuickCheck.DynamicLogic.Internal
import Test.QuickCheck.Monadic
import Test.QuickCheck.StateModel
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

data Counter = Counter {counter :: Int} deriving (Show, Generic)

instance StateModel Counter where
  data Action Counter a where
    Incr :: Action Counter ()
    Assert :: Int -> Action Counter ()

  arbitraryAction _ _ = pure $ Some Incr

  initialState = Counter 0

  nextState (Counter i) Incr _ = Counter (i + 1)
  nextState s Assert{} _ = s

  shrinkAction _ _ Incr = []
  shrinkAction _ _ (Assert i) = trace ("shrinking assert " <> show i) $ [Some (Assert i') | i' <- QC.shrink i]

deriving instance Eq (Action Counter a)
deriving instance Show (Action Counter a)

instance RunModel Counter Identity where
  perform _ Incr _ = return ()
  perform _ Assert{} _ = return ()

  postcondition _ Incr _ _ = pure True
  postcondition _ (Assert i) _ _ = pure (i < 2)

instance DynLogicModel Counter where
  restricted Assert{} = trace "restricting assert" $ True
  restricted _ = False

instance HasVariables (Action Counter a) where
  getAllVariables = const mempty

prop_Counter :: Actions Counter -> Property
prop_Counter acts =
  monadic runIdentity $ (True <$) $ runActions acts

dl = do
  anyActions_
  onState (Assert . counter)
  return ()

badProp :: Property
badProp = flip forAllDL prop_Counter dl

tests :: TestTree
tests =
  testGroup "Shrink dependent actions" [testProperty "should shrink to a minimal counterexample" badProp]

test = do
  let dynformula = runDL initialAnnotatedState dl
      dynlogic = unDynFormula dynformula 10
  dlTest <- QC.generate $ generate chooseNextStep dynlogic 1 initialAnnotatedState 10
  let shrunk = shrinkScript dynlogic (getScript dlTest)
      pruned = pruneDLTest dynlogic <$> shrunk
      tested = makeTestFromPruned dynlogic <$> pruned

  print $ dlTest
  print $ take 5 tested
  verboseCheck $ forAllScripts dynformula prop_Counter

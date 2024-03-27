{-# LANGUAGE TemplateHaskell #-}

module Issue31 where

import Control.Monad.Identity

import Test.QuickCheck
import Test.QuickCheck.DynamicLogic
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

deriving instance Eq (Action Counter a)
deriving instance Show (Action Counter a)

instance RunModel Counter Identity where
  perform _ Incr _ = return ()
  perform _ Assert{} _ = return ()

  postcondition _ Incr _ _ = pure True
  postcondition _ (Assert i) _ _ = pure (i < 2)

instance DynLogicModel Counter where
  restricted Assert{} = True
  restricted _ = False

instance HasVariables (Action Counter a) where
  getAllVariables = const mempty

prop_Counter :: Actions Counter -> Property
prop_Counter acts =
  monadic runIdentity $ (True <$) $ runActions acts

badProp :: Property
badProp = flip forAllDL prop_Counter $ do
  anyActions_
  i <- counter <$> getModelStateDL
  action $ Assert i
  return ()

tests :: TestTree
tests =
  testGroup "Shrink dependent actions" [testProperty "should shrink to a minimal counterexample" badProp]

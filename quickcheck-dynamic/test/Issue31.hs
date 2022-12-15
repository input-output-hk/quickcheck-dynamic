{-# LANGUAGE TemplateHaskell #-}
module Issue31 where

import Control.Monad.Identity

import Test.QuickCheck.DynamicLogic
import Test.QuickCheck.StateModel
import Test.QuickCheck
import Test.QuickCheck.Monadic

data Counter = Counter { counter :: Int } deriving (Show, Generic)

deriving instance Show (Action Counter a)

instance StateModel Counter where
  data Action Counter a where
    Incr   :: Action Counter ()
    Assert :: Int -> Action Counter ()

  arbitraryAction _ _ = pure $ Some Incr

  initialState = Counter 0

  nextState (Counter i) Incr _ = Counter (i + 1)
  nextState s Assert{} _ = s

instance RunModel Counter Identity where
  perform _ Incr _     = return ()
  perform _ Assert{} _ = return ()

  postcondition _ Incr _ _       = pure True
  postcondition _ (Assert i) _ _ = pure (i < 2)

instance DynLogicModel Counter where
  restricted Assert{} = True
  restricted _ = False

prop_Counter :: Actions Counter -> Property
prop_Counter = monadic runIdentity . (True <$) . runActions

badProp :: Property
badProp = flip forAllDL prop_Counter $ do
  anyActions_
  i <- fmap counter $ getModelStateDL
  action $ Assert i
  return ()

makeActionInstances ''Counter

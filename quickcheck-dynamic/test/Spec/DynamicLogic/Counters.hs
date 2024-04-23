{-# LANGUAGE NamedFieldPuns #-}

-- | Define several variant models of /counters/ which are useful to
-- test or use examples for various behaviours of the runtime.
module Spec.DynamicLogic.Counters where

import Control.Monad.Reader
import Data.IORef
import Test.QuickCheck
import Test.QuickCheck.StateModel

-- A very simple model with a single action that always succeed in
-- predictable way. This model is useful for testing the runtime.
newtype SimpleCounter (phase :: Phase) = SimpleCounter {count :: Int}
  deriving (Eq, Show, Generic)

deriving instance Eq (Action SimpleCounter phase a)
deriving instance Show (Action SimpleCounter phase a)
instance HasVariables (Action SimpleCounter Symbolic a) where
  getAllVariables _ = mempty

instance StateModel SimpleCounter where
  data Action SimpleCounter phase a where
    IncSimple :: Action SimpleCounter phase Int

  arbitraryAction _ _ = pure $ Some IncSimple

  initialState = SimpleCounter 0

  nextState SimpleCounter{count} IncSimple _ = SimpleCounter (count + 1)

instance RunModel SimpleCounter (ReaderT (IORef Int) IO) where
  perform IncSimple = do
    ref <- ask
    lift $ atomicModifyIORef' ref (\count -> (succ count, count))

  toDynAction IncSimple _ = IncSimple

-- A very simple model with a single action whose postcondition fails in a
-- predictable way. This model is useful for testing the runtime.
newtype FailingCounter (phase :: Phase) = FailingCounter {failingCount :: Int}
  deriving (Eq, Show, Generic)

deriving instance Eq (Action FailingCounter Symbolic a)
deriving instance Show (Action FailingCounter phase a)
instance HasVariables (Action FailingCounter Symbolic a) where
  getAllVariables _ = mempty

instance StateModel FailingCounter where
  data Action FailingCounter phase a where
    Inc' :: Action FailingCounter phase Int

  arbitraryAction _ _ = pure $ Some Inc'

  initialState = FailingCounter 0

  nextState FailingCounter{failingCount} Inc' _ = FailingCounter (failingCount + 1)

instance RunModel FailingCounter (ReaderT (IORef Int) IO) where
  perform Inc' = do
    ref <- ask
    lift $ atomicModifyIORef' ref (\count -> (succ count, count))

  toDynAction Inc' _ = Inc'

  postcondition (_, FailingCounter{failingCount}) _ _ = property $ failingCount < 4

-- A generic but simple counter model
data Counter (phase :: Phase) = Counter Int
  deriving (Show, Generic)

deriving instance Show (Action Counter phase a)
deriving instance Eq (Action Counter phase a)
instance HasVariables (Action Counter Symbolic a) where
  getAllVariables _ = mempty

instance StateModel Counter where
  data Action Counter phase a where
    Inc :: Action Counter phase ()
    Reset :: Action Counter phase Int

  initialState = Counter 0

  arbitraryAction _ _ = frequency [(5, pure $ Some Inc), (1, pure $ Some Reset)]

  nextState (Counter n) Inc _ = Counter (n + 1)
  nextState _ Reset _ = Counter 0

instance RunModel Counter (ReaderT (IORef Int) IO) where
  perform Inc = do
    ref <- ask
    lift $ modifyIORef ref succ
  perform Reset = do
    ref <- ask
    lift $ do
      n <- readIORef ref
      writeIORef ref 0
      pure n

  toDynAction Inc _ = Inc
  toDynAction Reset _ = Reset

  postcondition (Counter n, _) Reset res = n === res
  postcondition _ _ _ = property True

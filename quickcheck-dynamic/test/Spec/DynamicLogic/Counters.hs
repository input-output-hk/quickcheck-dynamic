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
newtype SimpleCounter = SimpleCounter {count :: Int}
  deriving (Eq, Show, Generic)

deriving instance Eq (Action SimpleCounter a)
deriving instance Show (Action SimpleCounter a)
instance HasVariables (Action SimpleCounter a) where
  getAllVariables _ = mempty

instance StateModel SimpleCounter where
  data Action SimpleCounter a where
    IncSimple :: Action SimpleCounter Int

  arbitraryAction _ _ = pure $ Some IncSimple

  initialState = SimpleCounter 0

  nextState SimpleCounter{count} IncSimple _ = SimpleCounter (count + 1)

instance RunModel SimpleCounter (ReaderT (IORef Int) IO) where
  perform _ IncSimple _ = do
    ref <- ask
    lift $ atomicModifyIORef' ref (\count -> (succ count, count))

  perform' IncSimple _ = do
    ref <- ask
    lift $ atomicModifyIORef' ref (\count -> (succ count, count))

-- A very simple model with a single action whose postcondition fails in a
-- predictable way. This model is useful for testing the runtime.
newtype FailingCounter = FailingCounter {failingCount :: Int}
  deriving (Eq, Show, Generic)

deriving instance Eq (Action FailingCounter a)
deriving instance Show (Action FailingCounter a)
instance HasVariables (Action FailingCounter a) where
  getAllVariables _ = mempty

instance StateModel FailingCounter where
  data Action FailingCounter a where
    Inc' :: Action FailingCounter Int

  arbitraryAction _ _ = pure $ Some Inc'

  initialState = FailingCounter 0

  nextState FailingCounter{failingCount} Inc' _ = FailingCounter (failingCount + 1)

instance RunModel FailingCounter (ReaderT (IORef Int) IO) where
  perform _ Inc' _ = do
    ref <- ask
    lift $ atomicModifyIORef' ref (\count -> (succ count, count))

  perform' Inc' _ = do
    ref <- ask
    lift $ atomicModifyIORef' ref (\count -> (succ count, count))

  postcondition (_, FailingCounter{failingCount}) _ _ _ = pure $ failingCount < 4

-- A generic but simple counter model
data Counter = Counter Int
  deriving (Show, Generic)

deriving instance Show (Action Counter a)
deriving instance Eq (Action Counter a)
instance HasVariables (Action Counter a) where
  getAllVariables _ = mempty

instance StateModel Counter where
  data Action Counter a where
    Inc :: Action Counter ()
    Reset :: Action Counter Int

  initialState = Counter 0

  arbitraryAction _ _ = frequency [(5, pure $ Some Inc), (1, pure $ Some Reset)]

  nextState (Counter n) Inc _ = Counter (n + 1)
  nextState _ Reset _ = Counter 0

instance RunModel Counter (ReaderT (IORef Int) IO) where
  perform _ Inc _ = do
     ref <- ask
     lift $ modifyIORef ref succ
  perform _ Reset _ = do
     ref <- ask
     lift $ do
       n <- readIORef ref
       writeIORef ref 0
       pure n

  perform' Inc _ = do
    ref <- ask
    lift $ atomicModifyIORef' ref ((,()) . succ)
  perform' Reset _ = do
    ref <- ask
    lift $ atomicModifyIORef' ref (\n -> (0, n))

  postcondition (Counter n, _) Reset _ res = pure $ n == res
  postcondition _ _ _ _ = pure True

{-# LANGUAGE NamedFieldPuns #-}

module Test.QuickCheck.Counters where

import Control.Monad.Reader (ReaderT, ask, lift)
import Data.IORef (IORef, atomicModifyIORef')
import Test.QuickCheck.StateModel (
  Any (..),
  Generic,
  HasVariables (..),
  RunModel (..),
  StateModel (..),
 )

newtype Counter = Counter {count :: Int}
  deriving (Eq, Show, Generic)

deriving instance Eq (Action Counter a)
deriving instance Show (Action Counter a)
instance HasVariables (Action Counter a) where
  getAllVariables _ = mempty

instance StateModel Counter where
  data Action Counter a where
    Inc :: Action Counter Int

  arbitraryAction _ _ = pure $ Some Inc

  initialState = Counter 0

  nextState Counter{count} Inc _ = Counter (count + 1)

instance RunModel Counter (ReaderT (IORef Int) IO) where
  perform _ Inc _ = do
    ref <- ask
    lift $ atomicModifyIORef' ref (\count -> (succ count, count))

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

  postcondition (_, FailingCounter{failingCount}) _ _ _ = pure $ failingCount < 4

{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Define several variant models of /counters/ which are useful to
-- test or use examples for various behaviours of the runtime.
module Spec.DynamicLogic.Counters where

import Control.Monad.Reader
import Test.QuickCheck hiding (Some)
import Test.QuickCheck.Extras
import Test.QuickCheck.Gen.Unsafe
import Test.QuickCheck.Monadic
import Test.QuickCheck.ParallelActions
import Test.QuickCheck.StateModel

import Control.Concurrent.Class.MonadSTM
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadTest
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTimer
import Control.Monad.IOSim

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

instance RunModel SimpleCounter (ReaderT (CounterState IO) IO) where
  perform _ IncSimple _ = do
    ref <- asks counterState
    lift $ atomically $ do
      c <- readTVar ref
      writeTVar ref (c + 1)
      return c

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

instance RunModel FailingCounter (ReaderT (CounterState IO) IO) where
  perform _ Inc' _ = do
    ref <- asks counterState
    lift $ atomically $ do
      c <- readTVar ref
      writeTVar ref (c + 1)
      return c

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

newtype CounterState m = CounterState {counterState :: TVar m Int}

setupCounterState :: MonadSTM m => m (CounterState m)
setupCounterState = CounterState <$> atomically (newTVar 0)

instance (MonadSTM m) => RunModel Counter (ReaderT (CounterState m) m) where
  perform _ Inc _ = do
    ref <- asks counterState
    lift $ do
      n <- atomically $ readTVar ref
      atomically $ writeTVar ref (n + 1)
  perform _ Reset _ = do
    ref <- asks counterState
    lift $ do
      n <- atomically $ readTVar ref
      atomically $ writeTVar ref 0
      pure n

  postcondition (Counter n, _) Reset _ res = pure $ n == res
  postcondition _ _ _ _ = pure True

instance MonadSTM m => RunModelPar Counter (ReaderT (CounterState m) m) where
  performPar Inc _ = do
    ref <- asks counterState
    -- lift $ atomically $ modifyTVar ref (\ c -> (c+1, ()))
    lift $ do
      n <- atomically $ readTVar ref
      atomically $ writeTVar ref (n + 1)
  performPar Reset _ = do
    ref <- asks counterState
    lift $ atomically $ do
      c <- readTVar ref
      writeTVar ref 0
      return c

prop_counter :: Actions Counter -> Property
prop_counter as = monadicIO $ do
  ref <- lift $ atomically $ newTVar (0 :: Int)
  runPropertyReaderT (runActions as) (CounterState ref :: CounterState IO)
  assert True

prop_counter_par :: ParallelActions Counter -> Property
prop_counter_par as = always 10 $ monadicIO $ do
  ref <- lift setupCounterState
  runPropertyReaderT (runParActions as) ref

prop_counter_parIOSimPor :: ParallelActions Counter -> Property
prop_counter_parIOSimPor as =
  monadicIOSimPOR_ prop
  where
    prop :: forall s. PropertyM (IOSim s) ()
    prop = do
      ref <- lift $ atomically $ newTVar (0 :: Int)
      lift $ exploreRaces
      runPropertyReaderT (runParActions as) (CounterState ref :: CounterState (IOSim s))

monadicIOSimPOR_ :: Testable a => (forall s. PropertyM (IOSim s) a) -> Property
monadicIOSimPOR_ prop = forAllBlind prop' $ \p -> exploreSimTrace id p $ \_ tr ->
  either (flip counterexample False . show) id $ traceResult False tr
  where
    prop' :: Gen (forall s. IOSim s Property)
    prop' = do
      Capture eval <- capture
      pure $ eval $ monadic' prop

instance Forking (IOSim s) where
  forkThread io = do
    t <- atomically newEmptyTMVar
    forkIO $ io >>= atomically . putTMVar t
    return $ atomically $ takeTMVar t

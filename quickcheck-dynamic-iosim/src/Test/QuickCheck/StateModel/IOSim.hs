{-# LANGUAGE UndecidableInstances #-}

module Test.QuickCheck.StateModel.IOSim where

import Control.Concurrent
import Control.Concurrent.Class.MonadSTM.TMVar qualified as IOClass
import Control.Concurrent.Class.MonadSTM.TVar qualified as IOClass
import Control.Concurrent.STM
import Control.Exception (SomeException (..))
import Control.Monad.Class.MonadFork qualified as IOClass
import Control.Monad.IOSim

import Test.QuickCheck
import Test.QuickCheck.Gen.Unsafe (Capture (Capture), capture)
import Test.QuickCheck.Monadic
import Test.QuickCheck.StateModel

type family RealizeIOSim s a where
  RealizeIOSim s ThreadId = IOClass.ThreadId (IOSim s)
  RealizeIOSim s (TVar a) = IOClass.TVar (IOSim s) a
  RealizeIOSim s (MVar a) = IOClass.TMVar (IOSim s) a
  RealizeIOSim s (f a b) = f (RealizeIOSim s a) (RealizeIOSim s b)
  RealizeIOSim s (f a) = f (RealizeIOSim s a)
  RealizeIOSim s a = a

type instance Realized (IOSim s) a = RealizeIOSim s a

runIOSimProperty :: (Testable a) => (forall s. PropertyM (IOSim s) a) -> Gen (SimTrace Property, Property)
runIOSimProperty p = do
  Capture eval <- capture
  let tr = runSimTrace (eval (monadic' p))
  case traceResult False tr of
    Right x ->
      pure (tr, x)
    Left (FailureException (SomeException ex)) ->
      pure (tr, counterexample (show ex) False)
    Left ex ->
      pure (tr, counterexample (show ex) False)

runIOSimProperty_ :: (Testable a) => (forall s. PropertyM (IOSim s) a) -> Gen Property
runIOSimProperty_ p = fmap snd $ runIOSimProperty p

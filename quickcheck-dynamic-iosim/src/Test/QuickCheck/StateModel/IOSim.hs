{-# LANGUAGE UndecidableInstances #-}

module Test.QuickCheck.StateModel.IOSim where

import Control.Concurrent
import Control.Concurrent.Class.MonadSTM.TMVar qualified as IOClass
import Control.Concurrent.Class.MonadSTM.TVar qualified as IOClass
import Control.Concurrent.STM
import Control.Monad.Class.MonadFork qualified as IOClass
import Control.Monad.IOSim

import Test.QuickCheck.StateModel

type family RealizeIOSim s a where
  RealizeIOSim s ThreadId = IOClass.ThreadId (IOSim s)
  RealizeIOSim s (TVar a) = IOClass.TVar (IOSim s) a
  RealizeIOSim s (MVar a) = IOClass.TMVar (IOSim s) a
  RealizeIOSim s (f a b) = f (RealizeIOSim s a) (RealizeIOSim s b)
  RealizeIOSim s (f a) = f (RealizeIOSim s a)
  RealizeIOSim s a = a

type instance Realized (IOSim s) a = RealizeIOSim s a

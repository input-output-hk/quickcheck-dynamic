{-# LANGUAGE UndecidableInstances #-}

module Test.QuickCheck.StateModel.IOSim where

import Control.Exception (SomeException (..))
import Control.Monad.IOSim

import Test.QuickCheck
import Test.QuickCheck.Gen.Unsafe (Capture (Capture), capture)
import Test.QuickCheck.Monadic

runIOSimProperty :: Testable a => (forall s. PropertyM (IOSim s) a) -> Gen (SimTrace Property, Property)
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

runIOSimProperty_ :: Testable a => (forall s. PropertyM (IOSim s) a) -> Gen Property
runIOSimProperty_ p = fmap snd $ runIOSimProperty p

module Spec.DynamicLogic.CounterModel where

import Control.Monad.Reader
import Data.IORef
import Test.QuickCheck
import Test.QuickCheck.Extras
import Test.QuickCheck.Monadic
import Test.QuickCheck.StateModel
import Test.Tasty hiding (after)
import Test.Tasty.QuickCheck

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

  postcondition (Counter n, _) Reset _ res = pure $ n == res
  postcondition _ _ _ _ = pure True

prop_counter :: Actions Counter -> Property
prop_counter as = monadicIO $ do
  ref <- lift $ newIORef (0 :: Int)
  runPropertyReaderT (runActions as) ref
  assert True

tests :: TestTree
tests =
  testGroup
    "counter tests"
    [ testProperty "prop_conter" $ prop_counter
    ]

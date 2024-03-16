{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- | An classic example of model-checking an implementation of a ring buffer.
--
-- * Inspired by [q-s-m example](https://github.com/stevana/quickcheck-state-machine/blob/master/test/CircularBuffer.hs)
-- * Also described by John Hughes [here](https://publications.lib.chalmers.se/records/fulltext/232550/local_232550.pdf)
module Spec.CircularBuffer.Model where

import Control.Monad.State (MonadState (..), StateT)
import Control.Monad.Trans (lift)
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import Spec.CircularBuffer.Buffer (Buffer, getBuffer, lenBuffer, newBuffer, putBuffer)
import Test.QuickCheck (Arbitrary (..), Property, getPositive, oneof)
import Test.QuickCheck.DynamicLogic (DL, DynLogicModel (..), forAllDL)
import Test.QuickCheck.Extras (runPropertyStateT)
import Test.QuickCheck.Monadic (monadicIO)
import Test.QuickCheck.Monadic qualified as QC
import Test.QuickCheck.StateModel (Actions, Any (..), HasVariables (..), RunModel (..), StateModel (..), Var, counterexamplePost, runActions)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

-- * Model

-- | A simple model of a `CircularBuffer` implemented using a `List`.
data CircularBufferModel
  = NoBuffer
  | CircularBufferModel
      { size :: Int
      , buffer :: [Var Int]
      }
  deriving (Eq, Show, Generic)

instance StateModel CircularBufferModel where
  data Action CircularBufferModel a where
    -- Create new buffer of given capacity.
    New :: Int -> Action CircularBufferModel ()
    -- Put an element at the top of the buffer.
    Put :: Int -> Action CircularBufferModel Int
    -- Get an element out of the bottom of the buffer.
    Get :: Action CircularBufferModel Int
    -- Get the number of elements in the buffer.
    Len :: Action CircularBufferModel Int

  arbitraryAction ctx NoBuffer = Some . New . getPositive <$> arbitrary
  arbitraryAction ctx CircularBufferModel{} =
    oneof
      [ Some . Put <$> arbitrary
      , pure $ Some Get
      , pure $ Some Len
      ]

  initialState = NoBuffer

  precondition NoBuffer New{} = True
  precondition CircularBufferModel{buffer} Get{} = length buffer > 0
  precondition CircularBufferModel{buffer, size} Put{} = length buffer < size
  precondition CircularBufferModel{} Len{} = True
  precondition _ _ = False

  nextState NoBuffer (New size) var = CircularBufferModel{size, buffer = mempty}
  nextState buf@CircularBufferModel{size, buffer} Put{} var = buf{buffer = var : buffer}
  nextState buf@CircularBufferModel{buffer} Get _ = buf{buffer = init buffer}
  nextState st _ _ = st

  shrinkAction _ _ = \case
    New n -> Some . New <$> [i | i <- shrink n, i > 0]
    Put n -> Some . Put <$> shrink n
    _ -> []

deriving instance Show (Action CircularBufferModel a)
deriving instance Eq (Action CircularBufferModel a)

instance HasVariables (Action CircularBufferModel a) where
  getAllVariables = const mempty

instance DynLogicModel CircularBufferModel where
  restricted _ = False

-- * RunModel

instance RunModel CircularBufferModel (StateT (Maybe Buffer) IO) where
  perform _st action _lookup =
    case action of
      New n -> lift (newBuffer n) >>= put . Just
      Put v -> get >>= (lift . putBuffer v . fromJust) >> pure v
      Get -> get >>= lift . getBuffer . fromJust
      Len -> get >>= lift . lenBuffer . fromJust

  postcondition (CircularBufferModel{buffer}, after) Get lookup res =
    let v = lookup (last buffer)
     in do
          counterexamplePost ("Expected: " <> show v <> ", got: " <> show res)
          pure $ v == res
  postcondition (CircularBufferModel{buffer}, after) Len lookup res = do
    let len = length buffer
    counterexamplePost $ "Expected; " <> show len <> ", got: " <> show res
    pure $ res == len
  postcondition _ _ _ _ = pure True

prop_CircularBuffer :: Actions CircularBufferModel -> Property
prop_CircularBuffer s =
  monadicIO $ do
    runPropertyStateT (runActions @_ @(StateT (Maybe Buffer) IO) s) Nothing
    QC.assert True

propDL :: DL CircularBufferModel () -> Property
propDL d = forAllDL d prop_CircularBuffer

tests :: TestTree
tests =
  testGroup
    "Circular Buffer"
    [testProperty "implementation respects its model" prop_CircularBuffer]

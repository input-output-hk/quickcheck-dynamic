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
import Test.QuickCheck (Arbitrary (..), Property, counterexample, getPositive, oneof)
import Test.QuickCheck.DynamicLogic (DL, DynLogicModel (..), action, anyActions_, assert, assertModel, forAllDL, getModelStateDL, monitorDL)
import Test.QuickCheck.Extras (runPropertyStateT)
import Test.QuickCheck.Monadic (monadicIO)
import Test.QuickCheck.Monadic qualified as QC
import Test.QuickCheck.StateModel (Actions, Any (..), HasVariables (..), RunModel (..), StateModel (..), Var, runActions)
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

  nextState NoBuffer (New size) var =
    CircularBufferModel{size, buffer = mempty}
  nextState buf@CircularBufferModel{size, buffer} Put{} var
    | length buffer < size = buf{buffer = var : buffer}
  nextState buf@CircularBufferModel{buffer} Get _
    | length buffer > 0 = buf{buffer = init buffer}
  nextState st _ _ = st

  precondition NoBuffer New{} = True
  precondition CircularBufferModel{size, buffer} Put{} = length buffer < size
  precondition CircularBufferModel{buffer} Get{} = length buffer > 0
  precondition _ _ = True

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
     in pure $ v == res
  postcondition _ _ _ _ = pure True

prop_CircularBuffer :: Actions CircularBufferModel -> Property
prop_CircularBuffer s =
  monadicIO $ do
    runPropertyStateT (runActions @_ @(StateT (Maybe Buffer) IO) s) Nothing
    QC.assert True

propDL :: DL CircularBufferModel () -> Property
propDL d = forAllDL d prop_CircularBuffer

-- probably not interesting = we are asserting something on the model
prop_NeverGoesOverSize :: DL CircularBufferModel ()
prop_NeverGoesOverSize = do
  anyActions_
  assertModel "Too many elements" $ \case
    NoBuffer -> True
    CircularBufferModel{size, buffer} -> length buffer <= size

prop_GetReturnsFirstPut :: DL CircularBufferModel ()
prop_GetReturnsFirstPut = do
  anyActions_
  getModelStateDL >>= \case
    CircularBufferModel{buffer} | not (null buffer) -> do
      let toGet = last buffer
      res <- action Get
      assert ("wrong element: " <> show toGet <> ", res: " <> show res) $ res == toGet
    _ -> pure ()

tests :: TestTree
tests =
  testGroup
    "Circular Buffer"
    [ testProperty "never has more than 'size' elements" $ propDL prop_NeverGoesOverSize
    , testProperty "Get first Put" $ propDL prop_GetReturnsFirstPut
    ]

-- | Monadic interface for writing /Dynamic Logic/ properties.
--
-- This interface offers a much nicer experience than manipulating the
-- expressions it is implemented on top of, especially as it improves
-- readability. It's still possible to express properties as pure
-- expressions using the `Test.QuickCheck.DynamicLogic.Core` module
-- and it might make sense depending on the context and the kind of
-- properties one wants to express.
module Test.QuickCheck.DynamicLogic (
  DL,
  action,
  anyAction,
  anyActions,
  anyActions_,
  stopping,
  weight,
  getSize,
  getModelStateDL,
  assert,
  assertModel,
  monitorDL,
  forAllQ,
  forAllDL,
  forAllDL_,
  forAllMappedDL,
  forAllMappedDL_,
  forAllUniqueDL,
  withDLTest,
  DL.DynLogic,
  DL.DynLogicModel (..),
  DL.DynLogicTest (..),
  DL.TestStep (..),
  module Test.QuickCheck.DynamicLogic.Quantify,
) where

import Control.Applicative
import Control.Monad
import Data.Typeable
import Test.QuickCheck hiding (getSize)
import Test.QuickCheck.DynamicLogic.Core qualified as DL
import Test.QuickCheck.DynamicLogic.Quantify
import Test.QuickCheck.StateModel

-- | The `DL` monad provides a nicer interface to dynamic logic formulae than the plain API.
--   It's a continuation monad producing a `DL.DynFormula` formula, with a state component threaded
--   through.
newtype DL s a = DL {unDL :: s -> (a -> s -> DL.DynFormula s) -> DL.DynFormula s}
  deriving (Functor)

instance Applicative (DL s) where
  pure x = DL $ \s k -> k x s
  (<*>) = ap

instance Alternative (DL s) where
  empty = DL $ \_ _ -> DL.ignore
  DL h <|> DL j = DL $ \s k -> h s k DL.||| j s k

instance Monad (DL s) where
  return = pure
  DL h >>= j = DL $ \s k -> h s $ \x s1 -> unDL (j x) s1 k

instance MonadFail (DL s) where
  fail = errorDL

action :: (Typeable a, Eq (Action s a)) => Action s a -> DL s ()
action cmd = DL $ \_ k -> DL.after cmd $ k ()

anyAction :: DL s ()
anyAction = DL $ \_ k -> DL.afterAny $ k ()

anyActions :: Int -> DL s ()
anyActions n =
  stopping
    <|> pure ()
    <|> (weight (fromIntegral n) >> anyAction >> anyActions n)

-- average number of actions same as average length of a list
anyActions_ :: DL s ()
anyActions_ = do
  n <- getSize
  anyActions (n `div` 2 + 1)

stopping :: DL s ()
stopping = DL $ \s k -> DL.toStop (k () s)

weight :: Double -> DL s ()
weight w = DL $ \s k -> DL.weight w (k () s)

getSize :: DL s Int
getSize = DL $ \s k -> DL.withSize $ \n -> k n s

getModelStateDL :: DL s s
getModelStateDL = DL $ \s k -> k s s

errorDL :: String -> DL s a
errorDL name = DL $ \_ _ -> DL.errorDL name

-- | Fail if the boolean is @False@.
--
--   Equivalent to
--
-- @
-- assert msg b = unless b (fail msg)
-- @
assert :: String -> Bool -> DL s ()
assert name b = if b then return () else errorDL name

assertModel :: String -> (s -> Bool) -> DL s ()
assertModel name p = assert name . p =<< getModelStateDL

monitorDL :: (Property -> Property) -> DL s ()
monitorDL f = DL $ \s k -> DL.monitorDL f (k () s)

-- | Generate a random value using the given `Quantification` (or list/tuple of quantifications).
--   Generated values will only shrink to smaller values that could also have been generated.
forAllQ :: Quantifiable q => q -> DL s (Quantifies q)
forAllQ q = DL $ \s k -> DL.forAllQ q $ \x -> k x s

runDL :: s -> DL s () -> DL.DynFormula s
runDL s dl = unDL dl s $ \_ _ -> DL.passTest

forAllUniqueDL ::
  (DL.DynLogicModel s, Testable a) =>
  Int ->
  s ->
  DL s () ->
  (Actions s -> a) ->
  Property
forAllUniqueDL nextVar initState d = DL.forAllUniqueScripts nextVar initState (runDL initState d)

forAllDL ::
  (DL.DynLogicModel s, Testable a) =>
  DL s () ->
  (Actions s -> a) ->
  Property
forAllDL d = DL.forAllScripts (runDL initialState d)

forAllDL_ ::
  (DL.DynLogicModel s, Testable a) =>
  DL s () ->
  (Actions s -> a) ->
  Property
forAllDL_ d = DL.forAllScripts_ (runDL initialState d)

forAllMappedDL ::
  (DL.DynLogicModel s, Testable a, Show rep) =>
  (rep -> DL.DynLogicTest s) ->
  (DL.DynLogicTest s -> rep) ->
  (Actions s -> srep) ->
  DL s () ->
  (srep -> a) ->
  Property
forAllMappedDL to from fromScript d prop =
  DL.forAllMappedScripts to from (runDL initialState d) (prop . fromScript)

forAllMappedDL_ ::
  (DL.DynLogicModel s, Testable a, Show rep) =>
  (rep -> DL.DynLogicTest s) ->
  (DL.DynLogicTest s -> rep) ->
  (Actions s -> srep) ->
  DL s () ->
  (srep -> a) ->
  Property
forAllMappedDL_ to from fromScript d prop =
  DL.forAllMappedScripts_ to from (runDL initialState d) (prop . fromScript)

withDLTest :: (DL.DynLogicModel s, Testable a) => DL s () -> (Actions s -> a) -> DL.DynLogicTest s -> Property
withDLTest d = DL.withDLScriptPrefix (runDL initialState d)

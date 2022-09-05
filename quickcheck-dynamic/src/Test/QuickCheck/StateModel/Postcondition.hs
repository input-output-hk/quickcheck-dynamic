-- | Result of evaluation the postcondition
--
-- See 'Test.QuickCheck.StateModel.postcondition'.
--
-- Intended for qualified import.
--
-- > import Test.QuickCheck.StateModel.Postcondition (Postcondition(..))
-- > import Test.QuickCheck.StateModel.Postcondition qualified as Post
module Test.QuickCheck.StateModel.Postcondition (
    Postcondition(..)
    -- * Primitives
  , assertSuccess
  , assertFailure
  , assertRelatedBy
  , assertBool
  , assertEQ
  , assertLE
  , assertLT
  , assertLeft
  , assertRight
    -- * Combinators
  , and
  , all
  , map
  ) where

import Prelude hiding (and, all, map)
import Prelude qualified

import Data.Bifunctor (bimap)
import Data.Foldable (toList)
import GHC.Show (appPrec1, showSpace)

-- | Result of 'postcondition'
newtype Postcondition = Postcondition { getPostcondition :: Either String () }
  deriving (Show)

{-------------------------------------------------------------------------------
  Primitives
-------------------------------------------------------------------------------}

assertSuccess :: Postcondition
assertSuccess = Postcondition $ Right ()

assertFailure :: String -> Postcondition
assertFailure = Postcondition . Left

assertBool :: String -> Bool -> Postcondition
assertBool _   True  = Postcondition $ Right ()
assertBool msg False = assertFailure msg

-- | Assert that two values are related by the given relation
--
-- The first argument should be a string representation representing the
-- negated relation. For example:
--
-- > assertEqual = assertRelatedBy "/=" (==)
assertRelatedBy :: Show a => String -> (a -> a -> Bool) -> a -> a -> Postcondition
assertRelatedBy op f x y = Postcondition $
    if f x y
      then Right ()
      else Left $ showsPrec appPrec1 x
                . showSpace
                . showString op
                . showSpace
                . showsPrec appPrec1 y
                $ ""

assertEQ :: (Eq a, Show a) => a -> a -> Postcondition
assertEQ = assertRelatedBy "/=" (==)

assertLT :: (Ord a, Show a) => a -> a -> Postcondition
assertLT = assertRelatedBy ">=" (<)

assertLE :: (Ord a, Show a) => a -> a -> Postcondition
assertLE = assertRelatedBy ">" (<=)

assertLeft :: Show b => Either a b -> Postcondition
assertLeft (Left  _)  = assertSuccess
assertLeft (Right b) = assertFailure $ "Expected Left: " ++ show b

assertRight :: Show a => Either a b -> Postcondition
assertRight (Right _) = assertSuccess
assertRight (Left  a) = assertFailure $ "Expected Right: " ++ show a

{-------------------------------------------------------------------------------
  Combinators
-------------------------------------------------------------------------------}

map :: (String -> String) -> Postcondition -> Postcondition
map f = Postcondition . bimap f id . getPostcondition

and :: Foldable t => t Postcondition -> Postcondition
and = Postcondition . sequence_ . Prelude.map getPostcondition . toList

all :: forall t a. (Foldable t, Show a) => (a -> Postcondition) -> t a -> Postcondition
all f = and . Prelude.map aux . toList
  where
    aux :: a -> Postcondition
    aux a = map (\msg -> show a ++ ": " ++ msg) (f a)

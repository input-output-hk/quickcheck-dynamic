{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | This module defines Quantifications, which are used together with
--   forAllQ in DynamicLogic. A `Quantification t` can be used to generate
--   a `t`, shrink a `t`, and recognise a generated `t`.
module Test.QuickCheck.DynamicLogic.Quantify (
  Quantification (isaQ),
  QuantifyConstraints,
  isEmptyQ,
  generateQ,
  shrinkQ,
  arbitraryQ,
  exactlyQ,
  elementsQ,
  oneofQ,
  frequencyQ,
  mapQ,
  whereQ,
  chooseQ,
  withGenQ,
  hasNoVariablesQ,
  validQuantification,
  Quantifiable (..),
) where

import Control.Monad
import Data.Coerce
import Data.Maybe
import Data.Typeable
import System.Random
import Test.QuickCheck
import Test.QuickCheck.DynamicLogic.CanGenerate
import Test.QuickCheck.StateModel

-- | A `Quantification` over a type @a@ is a generator that can be used to generate random values in
-- DL scenarios.
--
-- A `Quantification` is similar to a  `Test.QuickCheck.Arbitrary`, it groups together:
--
-- * A standard QuickCheck _generator_ in the `Gen` monad, which can be "empty",
-- * A _shrinking_ strategy for generated values in the case of a
--   failures ensuring they stay within the domain,
-- * A _predicate_ allowing finer grained control on generation
--   and shrinking process, e.g in the case the range of the generator
--   depends on trace context.
--
-- NOTE: Leaving the possibility of generating `Nothing` is useful to simplify the generation
-- process for `elements` or `frequency` which may normally crash when the list to select
-- elements from is empty. This makes writing `DL` formulas cleaner, removing the need to
-- handle non-existence cases explicitly.
data Quantification a = Quantification
  { genQ :: Maybe (Gen a)
  , isaQ :: a -> Bool
  , shrQ :: a -> [a]
  }

isEmptyQ :: Quantification a -> Bool
isEmptyQ = isNothing . genQ

generateQ :: Quantification a -> Gen a
generateQ q = fromJust (genQ q) `suchThat` isaQ q

shrinkQ :: Quantification a -> a -> [a]
shrinkQ q a = filter (isaQ q) (shrQ q a)

-- | Construct a `Quantification a` from its constituents.
-- Note the predicate provided is used to restrict both the range of values
-- generated and the list of possible shrinked values.
withGenQ :: Gen a -> (a -> Bool) -> (a -> [a]) -> Quantification a
withGenQ gen isA = Quantification (Just $ gen `suchThat` isA) isA

-- | Pack up an `Arbitrary` instance as a `Quantification`. Treats all values as being in range.
arbitraryQ :: Arbitrary a => Quantification a
arbitraryQ = Quantification (Just arbitrary) (const True) shrink

-- | Generates exactly the given value. Does not shrink.
exactlyQ :: Eq a => a -> Quantification a
exactlyQ a =
  Quantification
    (Just $ return a)
    (== a)
    (const [])

-- | Generate a random value in a given range (inclusive).
chooseQ :: (Arbitrary a, Random a, Ord a) => (a, a) -> Quantification a
chooseQ r@(a, b) =
  Quantification
    (guard (a <= b) >> Just (choose r))
    is
    (filter is . shrink)
  where
    is x = a <= x && x <= b

-- | Pick a random value from a list. Treated as an empty choice if the list is empty:
--
-- @
-- `Plutus.Contract.Test.ContractModel.forAllQ` (`elementsQ` []) == `Control.Applicative.empty`
-- @
elementsQ :: Eq a => [a] -> Quantification a
elementsQ as = Quantification g (`elem` as) (\a -> takeWhile (/= a) as)
  where
    g
      | null as = Nothing
      | otherwise = Just (elements as)

-- | Choose from a weighted list of quantifications. Treated as an `Control.Applicative.empty`
--   choice if no quantification has weight > 0.
frequencyQ :: [(Int, Quantification a)] -> Quantification a
frequencyQ iqs =
  Quantification
    ( case [(i, g) | (i, q) <- iqs, i > 0, Just g <- [genQ q]] of
        [] -> Nothing
        igs -> Just (frequency igs)
    )
    (isa iqs)
    (shr iqs)
  where
    isa [] _ = False
    isa ((i, q) : iqs) a = (i > 0 && isaQ q a) || isa iqs a
    shr [] _ = []
    shr ((i, q) : iqs) a =
      [a' | i > 0, isaQ q a, a' <- shrQ q a]
        ++ shr iqs a

-- | Choose from a list of quantifications. Same as `frequencyQ` with all weights the same (and >
--   0).
oneofQ :: [Quantification a] -> Quantification a
oneofQ qs = frequencyQ $ map (1,) qs

-- | `Quantification` is not a `Functor`, since it also keeps track of the range of the generators.
--   However, if you have two functions
-- @
-- to   :: a -> b
-- from :: b -> a
-- @
--   satisfying @from . to = id@ you can go from a quantification over @a@ to one over @b@. Note
--   that the @from@ function need only be defined on the image of @to@.
mapQ :: (a -> b, b -> a) -> Quantification a -> Quantification b
mapQ (f, g) q =
  Quantification
    ((f <$>) <$> genQ q)
    (isaQ q . g)
    (map f . shrQ q . g)

-- | Restrict the range of a quantification.
whereQ :: Quantification a -> (a -> Bool) -> Quantification a
whereQ q p =
  Quantification
    ( case genQ q of
        Just g | canGenerate 0.01 g p -> Just (g `suchThat` p)
        _ -> Nothing
    )
    (\a -> p a && isaQ q a)
    (\a -> if p a then filter p (shrQ q a) else [])

pairQ :: Quantification a -> Quantification b -> Quantification (a, b)
pairQ q q' =
  Quantification
    (liftM2 (,) <$> genQ q <*> genQ q')
    (\(a, a') -> isaQ q a && isaQ q' a')
    (\(a, a') -> map (,a') (shrQ q a) ++ map (a,) (shrQ q' a'))

-- | Wrap a Quantification in `HasNoVariables` to indicate that you know
-- what you're doing and there are no symbolic variables in the thing you
-- are quantifying over. WARNING: use this function carefully as there is
-- no guarantee that you won't get bitten by very strange failures if you
-- were in fact not honest about the lack of variables.
hasNoVariablesQ :: Quantification a -> Quantification (HasNoVariables a)
hasNoVariablesQ = coerce

type QuantifyConstraints a = (Eq a, Show a, Typeable a, HasVariables a)

-- | Generalization of `Quantification`s, which lets you treat lists and tuples of quantifications
--   as quantifications. For instance,
--
-- @
--   ...
--   (die1, die2) <- `Plutus.Contract.Test.ContractModel.forAllQ` (`chooseQ` (1, 6), `chooseQ` (1, 6))
--   ...
-- @
class
  QuantifyConstraints (Quantifies q) =>
  Quantifiable q
  where
  -- | The type of values quantified over.
  --
  -- @
  -- `Quantifies` (`Quantification` a) = a
  -- @
  type Quantifies q

  -- | Computing the actual `Quantification`.
  quantify :: q -> Quantification (Quantifies q)

instance QuantifyConstraints a => Quantifiable (Quantification a) where
  type Quantifies (Quantification a) = a
  quantify = id

instance (Quantifiable a, Quantifiable b) => Quantifiable (a, b) where
  type Quantifies (a, b) = (Quantifies a, Quantifies b)
  quantify (a, b) = pairQ (quantify a) (quantify b)

instance (Quantifiable a, Quantifiable b, Quantifiable c) => Quantifiable (a, b, c) where
  type Quantifies (a, b, c) = (Quantifies a, Quantifies b, Quantifies c)
  quantify (a, b, c) = mapQ (to, from) (quantify a `pairQ` (quantify b `pairQ` quantify c))
    where
      to (a, (b, c)) = (a, b, c)
      from (a, b, c) = (a, (b, c))

instance (Quantifiable a, Quantifiable b, Quantifiable c, Quantifiable d) => Quantifiable (a, b, c, d) where
  type
    Quantifies (a, b, c, d) =
      (Quantifies a, Quantifies b, Quantifies c, Quantifies d)
  quantify (a, b, c, d) =
    mapQ (to, from) (quantify a `pairQ` (quantify b `pairQ` (quantify c `pairQ` quantify d)))
    where
      to (a, (b, (c, d))) = (a, b, c, d)
      from (a, b, c, d) = (a, (b, (c, d)))

instance
  (Quantifiable a, Quantifiable b, Quantifiable c, Quantifiable d, Quantifiable e)
  => Quantifiable (a, b, c, d, e)
  where
  type
    Quantifies (a, b, c, d, e) =
      (Quantifies a, Quantifies b, Quantifies c, Quantifies d, Quantifies e)
  quantify (a, b, c, d, e) =
    mapQ (to, from) (quantify a `pairQ` (quantify b `pairQ` (quantify c `pairQ` (quantify d `pairQ` quantify e))))
    where
      to (a, (b, (c, (d, e)))) = (a, b, c, d, e)
      from (a, b, c, d, e) = (a, (b, (c, (d, e))))

instance Quantifiable a => Quantifiable [a] where
  type Quantifies [a] = [Quantifies a]
  quantify [] = Quantification (Just $ return []) null (const [])
  quantify (a : as) =
    mapQ (to, from) (pairQ (quantify a) (quantify as))
      `whereQ` (not . null)
    where
      to (x, xs) = x : xs
      from (x : xs) = (x, xs)
      from [] = error "quantify: impossible"

-- | Turns a `Quantification` into a `Property` to enable QuickChecking its
-- validity.
validQuantification :: Show a => Quantification a -> Property
validQuantification q =
  forAllShrink (fromJust $ genQ q) (shrinkQ q) $ isaQ q

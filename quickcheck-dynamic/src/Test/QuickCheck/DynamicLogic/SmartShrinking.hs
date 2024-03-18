module Test.QuickCheck.DynamicLogic.SmartShrinking (shrinkSmart) where

import Test.QuickCheck

-- | This combinator captures the 'smart shrinking' implemented for the
-- `Smart` type wrapper in [Test.QuickCheck.Modifiers](https://hackage.haskell.org/package/QuickCheck-2.14.3/docs/Test-QuickCheck-Modifiers.html#t:Smart).
-- It interleaves the output of the given shrinker to try to converge to more
-- interesting values faster.
shrinkSmart :: (a -> [a]) -> Smart a -> [Smart a]
shrinkSmart shrinker (Smart i x) = take i' ys `interleave` drop i' ys
  where
    ys = [Smart j y | (j, y) <- [0 ..] `zip` shrinker x]

    i' = 0 `max` (i - 2)

    [] `interleave` bs = bs
    as `interleave` [] = as
    (a : as) `interleave` (b : bs) = a : b : (as `interleave` bs)

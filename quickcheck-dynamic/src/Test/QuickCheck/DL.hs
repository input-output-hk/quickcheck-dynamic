{-# LANGUAGE FunctionalDependencies #-}

module Test.QuickCheck.DL where

import Control.Applicative ((<|>))
import Control.Monad (foldM)
import Data.Kind (Type)
import Test.QuickCheck (
  Arbitrary (..),
  Gen,
  Property,
  forAllShrink,
  (===),
 )

-- Definition of Propositional Dynamic Logic
-- p.164
-- f and p are atomic formulas and propositions respectively
-- See also these <lecture notes https://www.cs.cmu.edu/~fp/courses/15816-s10/lectures/19-PDL.pdf>

-- | `Prop`ositional logic part of Dynamic Logic.
-- This type is parameterised by the type of atomic propositions and atomic programs, whose
-- semantics is given by a Kripke structure.
data Prop f p where
  -- | Atomic propositions
  Prop :: f -> Prop f p
  -- | Falsity
  Zero :: Prop f p
  -- | Truth
  One :: Prop f p
  -- | Basic logic combinator.
  --
  -- Classically it's possible to reconstruct all the other usual
  -- combinators from implication and falsity.
  Imply :: Prop f p -> Prop f p -> Prop f p
  -- | Necessity modality
  --
  -- This is the only modality in this presentation as /possibility/
  -- is cumbersome to implement.
  Always :: Prog f p -> Prop f p -> Prop f p
  deriving (Eq, Show)

-- | `Prog`ams part of Dynamic Logic.
--
-- This type is parameterised by the type of atomic propositions and atomic programs, whose
-- semantics is typically given by a Kripke structure.
data Prog f p where
  -- | Empty program, or /Stop/.
  Empty :: Prog f p
  -- | Atomic programs.
  Prog :: p -> Prog f p
  -- | Sequencing of programs.
  Seq :: Prog f p -> Prog f p -> Prog f p
  -- | Alternation of programs.
  Alt :: Prog f p -> Prog f p -> Prog f p
  -- | Unbounded repetition of programs.
  Star :: Prog f p -> Prog f p
  -- | Test of a `Prop`osition.
  Test :: Prop f p -> Prog f p
  deriving (Eq, Show)

step
  :: (f -> s -> Bool)
  -- ^ A function to evaluate atomic formulas
  -> (p -> s -> [s])
  -- ^ State transition relation
  -> Int
  -- ^ Size limit (?)
  -> Prog f p
  -- ^ Program to interpret
  -> s
  -- ^ state
  -> [s]
step _ _ 0 _ _ = []
step ϕ δ k prog s =
  case prog of
    Empty -> [s]
    Prog p -> δ p s
    Seq p q -> do
      s' <- step ϕ δ k p s
      step ϕ δ k q s'
    Alt p q -> do
      step ϕ δ k p s <|> step ϕ δ k q s
    Test f ->
      [s | satisfy ϕ δ (k - 1) f s]
    Star p ->
      foldM (flip $ step ϕ δ (k - 1)) s (replicate k p)

-- | Satisfiability of a given formula in  `Prop f p` in a given state `s` against
-- a Kripke structure.
--
-- The size limit `k` is needed to limit the length of `Star` expressions and
-- the depth of expression analyzed.
satisfy
  :: (f -> s -> Bool)
  -- ^ A function to evaluate atomic formulas
  -> (p -> s -> [s])
  -- ^ Transition relation for programs
  -> Int
  -- ^ Size limit (?)
  -> Prop f p
  -- ^ The proposition to evaluate
  -> s
  -- ^ Initial state
  -> Bool
satisfy ϕ δ k prop s =
  case prop of
    Prop f -> ϕ f s
    Zero -> False
    One -> True
    Imply f g ->
      not (satisfy ϕ δ (k - 1) f s) || satisfy ϕ δ (k - 1) g s
    Always p f ->
      case step ϕ δ k p s of
        [] -> False
        states -> all (satisfy ϕ δ (k - 1) f) states

-- example from p.170

data S = U | V | W
  deriving (Eq, Show)

data P = A
  deriving (Eq, Show)

instance Test.QuickCheck.Arbitrary P where
  arbitrary = pure A

type F = [S]

phi :: F -> S -> Bool
phi = flip elem

delta :: P -> S -> [S]
delta A = \case
  U -> [V, W]
  W -> [V]
  V -> [W]

-- | Evaluates some `Prop`osition given some state `s` and a
-- finite /computation sequence/ `[p]`.
eval
  :: (Eq s, Show p, Show f)
  => (f -> s -> Bool)
  -- ^ A function to evaluate atomic formulas
  -> (p -> s -> Maybe s)
  -- ^ Partial transition function for programs
  -> s
  -- ^ Initial state
  -> Prop f p
  -- ^ The proposition to evaluate
  -> [p]
  -- ^ The trace to check satisfaction of proposition against
  -> Prop f p
eval ϕ δ s prop trace =
  case prop of
    Prop f ->
      if ϕ f s then One else Zero
    Zero -> Zero
    One -> One
    Imply f g ->
      let f' = eval ϕ δ s f trace
          g' = eval ϕ δ s g trace
       in case (f', g') of
            (Zero, _) -> One
            (One, One) -> One
            (_, _) -> Zero
    Always p f ->
      case match ϕ δ s trace p of
        Right (s', trace') -> eval ϕ δ s' f trace'
        Left{} -> Zero

match
  :: (Show p, Eq s, Show f)
  => (f -> s -> Bool)
  -> (p -> s -> Maybe s)
  -> s
  -> [p]
  -> Prog f p
  -> Either (String, [p]) (s, [p])
match _ _ s [] = \case
  Empty -> Right (s, [])
  Star{} -> Right (s, [])
  other -> Left ("empty trace with program " <> show other, [])
match ϕ δ s trace@(a : as) = \case
  Empty -> Left ("program should terminate", trace)
  Prog b ->
    case (δ a s, δ b s) of
      (Just t, Just t')
        | t == t' -> Right (t', as)
      _other ->
        Left ("unmatched atomic program, expected" <> show b <> ", found " <> show a, trace)
  Seq p q -> do
    (s', trace') <- match ϕ δ s trace p
    match ϕ δ s' trace' q
  Alt p q ->
    case match ϕ δ s trace p of
      Right v -> Right v
      Left{} -> match ϕ δ s trace q
  Star p' ->
    case match ϕ δ s trace p' of
      Right (s', trace') -> match ϕ δ s' trace' (Star p')
      Left{} -> Right (s, trace)
  Test f ->
    case eval ϕ δ s f trace of
      One -> Right (s, trace)
      _other -> Left ("test failed", trace)

isSatisfiable
  :: (Show p, Eq s, Show f, Eq f, Eq p, Test.QuickCheck.Arbitrary p)
  => (f -> s -> Bool)
  -- ^ The atomic propositions truth value
  -> (p -> s -> Maybe s)
  -- ^ Transition relation
  -> s
  -- ^ Initial state
  -> Test.QuickCheck.Gen [p]
  -- ^ A generator of traces from `s`
  -> Prop f p
  -- ^ A formula to check
  -> Test.QuickCheck.Property
isSatisfiable ϕ δ i gen f =
  Test.QuickCheck.forAllShrink gen Test.QuickCheck.shrink $ \trace ->
    eval ϕ δ i f trace Test.QuickCheck.=== One

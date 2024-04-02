---
title: Dynamic Logic
---

> {-# LANGUAGE FunctionalDependencies #-}
> {-# LANGUAGE InstanceSigs #-}

> module DL where

> import Control.Applicative ((<|>))
> import Control.Monad (foldM)
> import Data.Kind (Type)
> import Test.QuickCheck (
>   Arbitrary (..),
>   Gen,
>   Property,
>   forAllShrink,
>   (===),
>  )

= Propositional Dynamic Logic

This follows the presentation from the book [Dynamic
Logic](https://mitpress.mit.edu/9780262527668/dynamic-logic/) by
Harel, Kozen and Tiuryn, in particular from p.164 on. Those [lecture
notes](https://www.cs.cmu.edu/~fp/courses/15816-s10/lectures/19-PDL.pdf)
have also been useful when preparing these notes.

== Syntax

=== Propositions

This is the `Prop`ositional logic part of _Dynamic Logic_. This type is
parameterised by the type of atomic propositions and atomic programs,
whose semantics is given by a Kripke structure.

> data Prop f p where

Atomic propositions are drawn from the type `f`.

>   Prop :: f -> Prop f p

False and true are propositions. Note that `One` is not strictly necessary but has been added for convenience purpose.

>   Zero :: Prop f p
>   One :: Prop f p


The main logical combinator is implication. It's well known that it's
possible to reconstruct disjunction and conjunctions from implication.

>   Imply :: Prop f p -> Prop f p -> Prop f p

The most interesting operator is the modal _necessity_ operator which
informally states that some `Prop`osition must _always_ hold after the
execution of some `Prog`ram. The dual operator, _possibility_, which
states some proposition can hold after some program, could be derived
but is impractical to work with in the context of test case
generations and verification.

>   Always :: Prog f p -> Prop f p -> Prop f p
>   deriving (Eq, Show)

=== Programs

`Prog`rams are what makes the Dynamic Logic a modal logic,
obviously. This type is also parameterised by atomic propositions `f`
and atomic programs (aka. _actions_) `p`.

> data Prog f p where

The empty program, also called _skip_ or represented by the $\emptyset$ symbol, represents termination of a
program execution. We could also define `Abort` or divergence to
represent non-termination or invalid execution.

>   Empty :: Prog f p

Every atomic program `p` is of course a program.

>   Prog :: p -> Prog f p

Programs can be run in _sequence_, one after another

>   Seq :: Prog f p -> Prog f p -> Prog f p

Or one or the other can be run, non deterministically.

>   Alt :: Prog f p -> Prog f p -> Prog f p

Programs can also be repeated, indefinitely, using an analog to the Kleene star from regular lagnauges.

>   Star :: Prog f p -> Prog f p

Finally, a program can be a _test_ which does not change the state of
the system but either yields _skip_ or _abort_ depending on the result
of evaluating the given proposition.

>   Test :: Prop f p -> Prog f p
>   deriving (Eq, Show)

=== Deriving constructs

Standard constructs from imperative programming can be define from the basic combinators, like `if/then/else` or `while` instructions:

> ifThenElse :: Prop f p -> Prog f p -> Prog f p -> Prog f p
> ifThenElse cond ifTrue ifFalse =
>   Alt (Seq (Test cond) ifTrue)
>       (Seq (Test (Imply cond Zero)) ifFalse)

== Semantics

The semantics of Dynamic Logic formulas is given by a _Kripke_ structure $K = (W, \phi, \delta)$

> class Kripke m w | w -> m where
>   type Formula w :: Type
>   type Program w :: Type

with $\phi$ giving an interpretation of atomic propositions as a subset of
$W$. Note that this subset is here defined through a characteristic
function `w -> Bool`.

>   ϕ :: Formula w -> w -> Bool

And $\delta$ giving an interpretation of atomic programs. The usual
presentation is to consider $\delta$ as a relation over $W$, by making
the computation monadic we provide some flexibility of interpretation:
A function is `m = Identity`, a partial function if `m = Maybe`, a
relation if `m = [w]` or anything else.

>   δ :: Program w -> w -> m w

=== Satisfiability

> -- | Satisfiability of a given formula in  `Prop f p` in a given state `s` against
> -- a Kripke structure.
> --
> -- The size limit `k` is needed to limit the length of `Star` expressions and
> -- the depth of expression analyzed.
> satisfy
>   :: Kripke [] s
>   => Int
>   -- ^ Size limit (?)
>   -> Prop (Formula s) (Program s)
>   -- ^ The proposition to evaluate
>   -> s
>   -- ^ Initial state
>   -> Bool
> satisfy k prop s =
>   case prop of
>     Prop f -> ϕ f s
>     Zero -> False
>     One -> True
>     Imply f g ->
>       not (satisfy (k - 1) f s) || satisfy (k - 1) g s
>     Always p f ->
>       case step k p s of
>         [] -> False
>         states -> all (satisfy (k - 1) f) states

> step
>   :: Kripke [] s
>   => Int
>   -- ^ Size limit (?)
>   -> Prog (Formula s) (Program s)
>   -- ^ Program to interpret
>   -> s
>   -- ^ state
>   -> [s]
> step 0 _ _ = []
> step k prog s =
>   case prog of
>     Empty -> [s]
>     Prog p -> δ p s
>     Seq p q -> do
>       s' <- step k p s
>       step k q s'
>     Alt p q -> do
>       step k p s <|> step k q s
>     Test f ->
>       [s | satisfy (k - 1) f s]
>     Star p ->
>       foldM (flip $ step (k - 1)) s (replicate k p)


> -- example from p.170

> data S = U | V | W
>   deriving (Eq, Show)

> data P = A
>   deriving (Eq, Show)

> type F = [S]

> instance Kripke [] S where
>   type Formula S = F
>   type Program S = P

>   ϕ :: F -> S -> Bool
>   ϕ = flip elem

>   δ :: P -> S -> [S]
>   δ A = \case
>     U -> [V, W]
>     W -> [V]
>     V -> [W]

> instance Test.QuickCheck.Arbitrary P where
>   arbitrary = pure A

> -- | Evaluates some `Prop`osition given some state `s` and a
> -- finite /computation sequence/ `[p]`.
> eval
>   :: (Eq s, Show p, Show f)
>   => (f -> s -> Bool)
>   -- ^ A function to evaluate atomic formulas
>   -> (p -> s -> Maybe s)
>   -- ^ Partial transition function for programs
>   -> s
>   -- ^ Initial state
>   -> Prop f p
>   -- ^ The proposition to evaluate
>   -> [p]
>   -- ^ The trace to check satisfaction of proposition against
>   -> Prop f p
> eval ϕ δ s prop trace =
>   case prop of
>     Prop f ->
>       if ϕ f s then One else Zero
>     Zero -> Zero
>     One -> One
>     Imply f g ->
>       let f' = eval ϕ δ s f trace
>           g' = eval ϕ δ s g trace
>        in case (f', g') of
>             (Zero, _) -> One
>             (One, One) -> One
>             (_, _) -> Zero
>     Always p f ->
>       case match ϕ δ s trace p of
>         Right (s', trace') -> eval ϕ δ s' f trace'
>         Left{} -> Zero

> match
>   :: (Show p, Eq s, Show f)
>   => (f -> s -> Bool)
>   -> (p -> s -> Maybe s)
>   -> s
>   -> [p]
>   -> Prog f p
>   -> Either (String, [p]) (s, [p])
> match _ _ s [] = \case
>   Empty -> Right (s, [])
>   Star{} -> Right (s, [])
>   other -> Left ("empty trace with program " <> show other, [])
> match ϕ δ s trace@(a : as) = \case
>   Empty -> Left ("program should terminate", trace)
>   Prog b ->
>     case (δ a s, δ b s) of
>       (Just t, Just t')
>         | t == t' -> Right (t', as)
>       _other ->
>         Left ("unmatched atomic program, expected" <> show b <> ", found " <> show a, trace)
>   Seq p q -> do
>     (s', trace') <- match ϕ δ s trace p
>     match ϕ δ s' trace' q
>   Alt p q ->
>     case match ϕ δ s trace p of
>       Right v -> Right v
>       Left{} -> match ϕ δ s trace q
>   Star p' ->
>     case match ϕ δ s trace p' of
>       Right (s', trace') -> match ϕ δ s' trace' (Star p')
>       Left{} -> Right (s, trace)
>   Test f ->
>     case eval ϕ δ s f trace of
>       One -> Right (s, trace)
>       _other -> Left ("test failed", trace)

> isSatisfiable
>   :: (Show p, Eq s, Show f, Eq f, Eq p, Test.QuickCheck.Arbitrary p)
>   => (f -> s -> Bool)
>   -- ^ The atomic propositions truth value
>   -> (p -> s -> Maybe s)
>   -- ^ Transition relation
>   -> s
>   -- ^ Initial state
>   -> Test.QuickCheck.Gen [p]
>   -- ^ A generator of traces from `s`
>   -> Prop f p
>   -- ^ A formula to check
>   -> Test.QuickCheck.Property
> isSatisfiable ϕ δ i gen f =
>   Test.QuickCheck.forAllShrink gen Test.QuickCheck.shrink $ \trace ->
>     eval ϕ δ i f trace Test.QuickCheck.=== One

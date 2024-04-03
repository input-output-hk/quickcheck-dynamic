

> {-# LANGUAGE FunctionalDependencies #-}
> {-# LANGUAGE InstanceSigs #-}
> {-# LANGUAGE ScopedTypeVariables #-}

> module DL where

> import Control.Applicative ((<|>))
> import Control.Monad (foldM)
> import Data.Kind (Type)
> import Test.QuickCheck (
>     Arbitrary (..),
>     Gen,
>     Property,
>     forAllShrink,
>     (===), discard, forAllShrinkShow,
>    )

The goal of this document is to provide a gentle introduction to
_Dynamic Logic_ which is a form of modal logic that is used within
`quickcheck-dynamic` to give the user the ability to express and tests
stateful properties about their code. The intention is to provide an
intuition of how things work within the library on a simpler version
of the logic.


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

Given some Kripke structure $K$, a formula `f` is _satisfied_ in a
state `s` denoted as $K, s\models f$ if the formula is true in the
state `s`, which is defined inductively over the structure of `f`.

Note that we need to limit the size of the formula to avoid infinite recursion.

> satisfy

Note that `satisfy` is defined here with atomic programs defining  _relations_  over `s` and not functions, eg. a given program `p` can have multiple possible state outcomes.

>   :: Kripke [] s

The size parameter bounds the depth at which state is explored and the formula is evaluated.

>   => Int

The formula to evaluate, `f` is a proposition dependent on the atomic propositions and programs in $K$.

>   -> Prop (Formula s) (Program s)

The state in which to evaluate the formula.

>   -> s
>   -> Bool
> satisfy k prop s =
>   case prop of

Atomic propositions are evaluated by the characteristic function $\phi$.

>     Prop f -> ϕ f s

False, true, and implications are evaluated as expected for propositional logic operators.

>     Zero -> False
>     One -> True
>     Imply f g ->
>       not (satisfy (k - 1) f s) || satisfy (k - 1) g s

The _always_ operator is evaluated by checking that the proposition
holds in _all_ the states reachable from the current state by
executing the program `p`.

>     Always p f ->
>       case step k p s of
>         [] -> False
>         states -> all (satisfy (k - 1) f) states

The `step` function computes the set of states reachable from the current state by executing the program `p`.

> step :: Kripke [] s
>   => Int
>   -> Prog (Formula s) (Program s)
>   -> s
>   -> [s]

If the size limit is reached, the computation is considered "stuck" and yields no reachable state.

> step 0 _ _ = []

In other cases, reachable states are defined inductively over the structure of the program.

> step k prog s =
>   case prog of

If program should stop, simply return the current state.

>     Empty -> [s]

Atomic programs yield the result of the transition relation $\delta$.

>     Prog p -> δ p s

Sequencing of programs is done by executing the first program and then the second program on the resulting state(s).

>     Seq p q -> do
>       s' <- step k p s
>       step k q s'

Non-deterministic choice is done by executing either the first or the second program, with a bias towards the first program.

>     Alt p q -> do
>       step k p s <|> step k q s

Test programs are executed by checking if the proposition holds in the current state and returning the current state if it does.

>     Test f ->
>       [s | satisfy (k - 1) f s]

Finally, repetition of a program is done by iterating over the nested program `k` times.

>     Star p ->
>       foldM (flip $ step (k - 1)) s (replicate k p)

=== A "concrete" Example

The following is a concrete example of some simple data types and
Kripke structure to evaluate the satisfaction of a formula, drawn from p.170 of the book.

The system's state is simply an enumeration of 3 different states.

> data S = U | V | W
>   deriving (Eq, Show)

There's a single atomic program, `A`.

> data P = A
>   deriving (Eq, Show)

The atomic propositions are simply a list of states and a proposition
is true iff reached state is included in the list.

> type F = [S]


> instance Kripke [] S where
>   type Formula S = F
>   type Program S = P

>   ϕ :: F -> S -> Bool
>   ϕ = flip elem

The transition relation is the only interesting part of the Kripke structure.

>   δ :: P -> S -> [S]
>   δ A = \case
>     U -> [V, W]
>     W -> [V]
>     V -> [W]

Now, let's define a simple formula to check if the state `V` is reachable from the state `U` after executing the program `A`.

> formula :: Prop F P
> formula = Always (Star (Prog A)) (Prop [V,W])

We can now check if the formula is satisfied in the state `U` of the Kripke structure defined above within GHCi:

```
$ satisfy 10 formula U
True
```

= Property-Based Testing

Another way to look at the semantics of Dynamic Logic is to consider it in the context of traces, that is finite sequences of programs. Here formula's meaning is the set of traces that satisfy the formula.
This is particularly useful in the context of property-based testing: Given some Kripke structure and generators, we can generate traces and check if all traces satisfy a given formula.

== Checking satisfiability

The basic property we want to check is the satisfiability of some
formula against arbitrary valid traces part of the Kripke
structure. In other words, we want to explore the states of the Kripke
structure that are reachable throughsome sequence of execution of
programs, and check if the formula holds for all of them.

> isSatisfiable
>   :: ( Eq s, Show s
>      , Eq (Formula s), Show (Formula s)
>      , Eq (Program s), Show (Program s)
>      , Test.QuickCheck.Arbitrary (Program s)
>      , Kripke Maybe s)

Given some initial state,

>    => s

A generator for traces,

>    -> (s -> Test.QuickCheck.Gen [Program s])

and a formula to check,

>    -> Prop (Formula s) (Program s)
>    -> Test.QuickCheck.Property
> isSatisfiable i gen f =
>   Test.QuickCheck.forAllShrink (gen i) Test.QuickCheck.shrink $ \trace ->

It should hold that the formula evaluates to `True` given the trace and the initial state.

>     eval i f trace Test.QuickCheck.=== One

To do so, we need to `eval`uate a formula in a given state against some trace, yielding a simpler formula until we reach atomic propositions or ground truths.

> eval
>   :: (Kripke Maybe s, Eq s, Show s, Show (Program s), Show (Formula s))
>    => s
>    -> Prop (Formula s) (Program s)
>    -> [Program s]
>    -> Prop (Formula s) (Program s)
> eval s prop trace =

Evaluation proceeds recursively over the structure of the formula and the trace.

>   case prop of

Atomic propositions should hold in the current state

>     Prop f ->
>       if ϕ f s then One else Zero
>     Zero -> Zero
>     One -> One

Implications follow the usual rules of logic

>     Imply f g ->
>       let f' = eval s f trace
>           g' = eval s g trace
>        in case (f', g') of
>             (Zero, _) -> One
>             (One, One) -> One
>             (_, _) -> Zero

The _always_ operator requires advancing the state by executing the
program `p` and evaluating the formula `f` in the resulting state,
agains the resulting trace.

>     Always p f ->
>       case match s trace p of
>         Right (s', trace') -> eval s' f trace'
>         Left{} -> Zero

The match function advances the state by executing the program `p`:

> match
>   :: (Kripke Maybe s, Eq s, Show s, Show (Program s), Show (Formula s))
>    => s
>    -> [Program s]
>    -> Prog (Formula s) (Program s)

Note match can fail if the program is not executable in the current
state, in which case it returns some error message and the remaining
trace.

>    -> Either (String, [Program s]) (s, [Program s])

The base case is when the trace is empty, in which case the program
should terminate which is only possible if the program is empty, or it
can be repeated indefinitely which includes the case where it's not
repeated at all.

> match s [] = \case
>   Empty -> Right (s, [])
>   Star{} -> Right (s, [])
>   other -> Left ("empty trace with program " <> show other, [])

If the trace is not empty, we need to inductively walk through the trace and the program structure:

> match s trace@(a : as) = \case
>   Empty -> Left ("program should terminate", trace)

For atomic programs, we could check equality of the atomic programs and advance the state if they matched, but we choose instead to check equality of _observational behaviors_ of the programs, eg. whether or not the two programs yield the same state:

>   Prog b ->
>     case (δ a s, δ b s) of
>       (Just t, Just t')
>         | t == t' -> Right (t', as)
>       _other ->
>         Left ("unmatched atomic program, expected" <> show b <> ", found " <> show a, trace)

Sequential execution is straightforward, simply feeding the new state and trace to the next program

>   Seq p q -> do
>     (s', trace') <- match s trace p
>     match s' trace' q

Non-deterministic choice is also straightforward, trying to match the
first program and if it fails, trying the second program.  Note that
we cannot here use the `Alternative` operator `(<|>)` as `Either e`
is, somewhat surprisingly in the case of an "error" monad, not an
instance of `Alternative`.

>   Alt p q ->
>     case match s trace p of
>       Right v -> Right v
>       Left{} -> match s trace q

Repetition is also quite simple and terminates because the base case (`Prog p`) consumes the head of the trace.

>   Star p' ->
>     case match s trace p' of
>       Right (s', trace') -> match s' trace' (Star p')
>       Left{} -> Right (s, trace)

Finally, we can `Test` a formula whose result depends on call to `eval` with current state and trace.

>   Test f ->
>     case eval s f trace of
>       One -> Right (s, trace)
>       _other -> Left ("test failed", trace)

=== Example

The generator for our simple model is quite simple as there's a single action.

> instance Test.QuickCheck.Arbitrary P where
>   arbitrary = pure A

However we need a different Kripke structure because we want it to work in the `Maybe` monad, so let's wrap `S` and create the structure:

> newtype S' = S' { unS :: S }
>   deriving (Eq, Show)

> instance Kripke Maybe S' where
>   type Formula S' = F
>   type Program S' = P

>   ϕ f = ϕ f . unS

>   δ A = \case
>     S' U -> Just $ S' W
>     S' V -> Just $ S' W
>     S' W -> Just $ S' V

Then we can run QuickCheck over our example formula:

```
$ quickCheck $ isSatisfiable (S' U)  (const arbitrary) formula
*** Failed! Falsified (after 1 test):
[]
Zero /= One
```

Of course, the formula is wrong on an empty sequence of actions as we are in state `U`, but if we start from state `V` we can verify the property holds:

```
quickCheck $ isSatisfiable (S' V)  (const arbitrary) formula
+++ OK, passed 100 tests.
```

== Generating valid traces

In practice, what we are interested in with Dynamic Logic formulas is
rather the converse of what we have done so far: Inferring the subset of Kripke
structure that's a model for a given formula, for a given "universe of
discourse" consisting of the atomic propositions and programs.

Our generator thus takes an initial state, a formula and output a list
of `Program` that satisfy this formula. If there's no such list, for
example because some proposition is not valid in some state, then the
generated sequence is `discard`ed.

> generate ::
>      forall s.
>      Kripke Maybe s
>      => s
>      -> Prop (Formula s) (Program s)
>      -> Gen [Program s]
> generate i = go i []
>   where

As expected, the generator proceeds recursively on the structure of
the proposition, accumulating a trace as it walks through the syntax
tree.

>    go :: s -> [Program s] -> Prop (Formula s) (Program s) ->  Gen [Program s]
>    go s acc = \case

A formula that's not verified in the current state discards its
accumulated trace, otherwise it yields it.

>      Prop f -> if ϕ f s
>                 then pure acc
>                 else discard

False and True and logical implications are handled as expected,
respectively discarding their trace, returning it or testing both
formulas with the same state.

>      Zero -> discard
>      One -> pure acc
>      Imply f g -> do
>        tr <- go s acc f
>        go s (acc <> tr) g

Modal necessity tries to make some progress from given program and
checks the result formula in the new state.

>      Always p f -> do
>        (s', prog) <- progress s p
>        go s' (acc <> prog) f

Progress proceeds also structurally through a program's structure:

>    progress :: s -> Prog (Formula s) (Program s) -> Gen (s, [Program s])
>    progress s = \case

Empty program succeeds yielding an empty trace and unchanged state.

>      Empty -> pure (s, [])

An atomic program updates state and trace iff $\delta$ is defined in
the Kripke structure for the current state and program.

>      Prog p ->
>        case δ p s of
>          Nothing -> discard
>          Just s' -> pure (s', [p])

Sequence of programs accumulate state change and trace for both executed programs.

>      Seq p q -> do
>        (s', p') <- progress s p
>        (s'', q') <- progress s' q
>        pure (s'', p' <> q')

Choice is handled non-deterministically, flipping a coin and selecting
one of the branches to make progress. This assumes that should one
branch fail, the other one will ultimately be selected.

>      Alt p q -> do
>        b <- arbitrary
>        if b
>         then progress s p
>         else progress s q

A test leaves the state unchanged and depends on the result of evaluating the formula.

>      Test f ->
>        (s,) <$> go s [] f

Finally, iterative execution also flips a coin: If it fails, an empty
trace and unchanged state is returned, otherwise it progresses through
one step of the program and recursively calls itself with the result.

>      Star p -> do
>        b <- arbitrary
>        if b
>          then do
>            (s', p') <- progress s p
>            (s'', p'') <- progress s' (Star p)
>            pure (s'', p' <> p'')
>          else
>            pure (s, [])

We can actually verify our generator is consistent with the given formula, expressing that as a `Property`:

> generateConsistentWithEval ::
>         ( Eq s, Show s
>         , Eq (Formula s), Show (Formula s)
>         , Eq (Program s), Show (Program s)
>         , Test.QuickCheck.Arbitrary (Program s)
>         , Kripke Maybe s)
>         => s
>         -> Prop (Formula s) (Program s)
>         -> Property
> generateConsistentWithEval i f =
>   forAllShrinkShow (generate i f) shrink show $ \ trace ->
>      eval i f trace Test.QuickCheck.=== One

Running this property gives us:

```
$ Test.QuickCheck.quickCheck $ generateConsistentWithEval (S' U) formula
+++ OK, passed 100 tests; 78 discarded.
```

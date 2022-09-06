{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Simple (stateful) Model-Based Testing library for use with Haskell QuickCheck.
--
-- This module provides the basic machinery to define a `StateModel` from which /traces/ can
-- be generated and executed against some /actual/ implementation code to define monadic `Property`
-- to be asserted by QuickCheck.
module Test.QuickCheck.StateModel (
  StateModel (..),
  RunModel (..),
  Any (..),
  Step (..),
  LookUp,
  Var (..), -- we export the constructors so that users can construct test cases
  Actions (..),
  pattern Actions,
  EnvEntry (..),
  Env,
  Realized,
  stateAfter,
  runActions,
  runActionsInState,
  lookUpVar,
  lookUpVarMaybe,
) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Data
import Data.Kind
import Test.QuickCheck as QC
import Test.QuickCheck.DynamicLogic.SmartShrinking
import Test.QuickCheck.Monadic

-- | The typeclass users implement to define a model against which to validate some implementation.
--
-- To implement a `StateModel`, user needs to provide at least the following:
--
--   * A datatype for `Action`s: Each test case is a sequence of `Action`s that's supposed to lead from
--     some `initialState` to some end state,
--   * A generator for traces of `Action`s, the `arbitraryAction` function,
--   * An `initialState`,
--   * A /transition/ function, `nextState`, that "interprets" each `Action` and producing some new `state`.
--
-- For finer grained control over the testing process, one can also define:
--
--  * `shrinkAction`: Shrinking is an important part of MBT as it allows QuickCheck engine to look for simpler
--    test cases when something goes wrong which makes troubleshooting easier,
--  * `precondition`: Filters generated `Action` depending on the `state`. When `precondition` is False then
--    the action is /rejected/ and a new one is tried. This is also useful when shrinking a trace
--    in order to ensure that removing some `Action` still produces a valid trace. The `precondition` can be
--    somewhat redundant with the generator's conditions,
class
  ( forall a. Show (Action state a)
  , Show state
  ) =>
  StateModel state
  where
  -- | The type of `Action` relevant for this `state`.
  --
  -- This is expected to be defined as a GADT where the `a` parameter is instantiated to some
  -- observable output from the SUT a given action is expected to produce. For example, here
  -- is a fragment of the `Action RegState` (taken from the `Spec.Dynamic.RegistryModel`  module) :
  --
  -- @
  --   data Action RegState a where
  --     Spawn      ::                           Action RegState ThreadId
  --     Register   :: String -> Var ThreadId -> Action RegState (Either ErrorCall ())
  --     KillThread :: Var ThreadId           -> Action RegState ()
  -- @
  --
  -- The @Spawn@ action should produce a @ThreadId@, whereas the @KillThread@ action does not return
  -- anything.
  data Action state a

  -- | Display name for `Action`.
  -- This is useful to provide sensible statistics about the distribution of `Action`s run
  -- when checking a property.
  --
  -- Default implementation uses a poor-man's string manipulation method to extract the
  -- constructor name from the value.
  actionName :: Action state a -> String
  actionName = head . words . show

  -- | Generator for `Action` depending on `state`.
  -- The generated values are wrapped in `Any` type to allow the model to /not/ generate an action under
  -- some circumstances: Any generated  `Error` value will be ignored when generating a trace for testing.
  arbitraryAction :: state -> Gen (Any (Action state))

  -- | Shrinker for `Action`.
  -- Defaults to no-op but as usual, defining a good shrinker greatly enhances the usefulness
  -- of property-based testing.
  shrinkAction :: Typeable a => state -> Action state a -> [Any (Action state)]
  shrinkAction _ _ = []

  -- | Initial state of generated traces.
  initialState :: state

  -- | Transition function for the model.
  -- The `Var a` parameter is useful to keep reference to actual value of type `a` produced
  -- by `perform`ing the `Action` inside the `state` so that further actions can use `Lookup`
  -- to retrieve that data. This allows the model to be ignorant of those values yet maintain
  -- some references that can be compared and looked for.
  nextState :: Typeable a => state -> Action state a -> Var a -> state
  nextState s _ _ = s

  -- | Precondition for filtering generated `Action`.
  -- This function is applied before the action is performed, it is useful to refine generators that
  -- can produce more values than are useful.
  precondition :: state -> Action state a -> Bool
  precondition _ _ = True

-- TODO: maybe it makes sense to write
-- out a long list of these instances
type family Realized (m :: Type -> Type) a :: Type
type instance Realized IO a = a
type instance Realized (StateT s m) a = Realized m a
type instance Realized (ReaderT r m) a = Realized m a

class Monad m => RunModel state m where
  -- | Perform an `Action` in some `state` in the `Monad` `m`.  This
  -- is the function that's used to exercise the actual stateful
  -- implementation, usually through various side-effects as permitted
  -- by `m`. It produces a value of type `a`, eg. some observable
  -- output from the `Action` that should later be kept in the
  -- environment through a `Var a` also passed to the `nextState`
  -- function.
  --
  -- The `Lookup` parameter provides an /environment/ to lookup `Var
  -- a` instances from previous steps.
  perform :: forall a. Typeable a => state -> Action state a -> LookUp m -> m (Realized m a)

  -- | Postcondition on the `a` value produced at some step.
  -- The result is `assert`ed and will make the property fail should it be `False`. This is useful
  -- to check the implementation produces expected values.
  postcondition :: forall a. (state, state) -> Action state a -> LookUp m -> Realized m a -> m Bool
  postcondition _ _ _ _ = pure True

  -- | Allows the user to attach information to the `Property` at each step of the process.
  -- This function is given the full transition that's been executed, including the start and ending
  -- `state`, the `Action`, the current environment to `Lookup` and the value produced by `perform`
  -- while executing this step.
  monitoring :: forall a. (state, state) -> Action state a -> LookUp m -> Realized m a -> Property -> Property
  monitoring _ _ _ _ prop = prop

type LookUp m = forall a. Typeable a => Var a -> Realized m a

type Env m = [EnvEntry m]

data EnvEntry m where
  (:==) :: Typeable a => Var a -> Realized m a -> EnvEntry m

infix 5 :==

lookUpVarMaybe :: forall a m. Typeable a => Env m -> Var a -> Maybe (Realized m a)
lookUpVarMaybe [] _ = Nothing
lookUpVarMaybe (((v' :: Var b) :== a) : env) v =
  case eqT @a @b of
    Just Refl | v == v' -> Just a
    _ -> lookUpVarMaybe env v

lookUpVar :: Typeable a => Env m -> Var a -> Realized m a
lookUpVar env v = case lookUpVarMaybe env v of
  Nothing -> error $ "Variable " ++ show v ++ " is not bound!"
  Just a -> a

data Any f where
  Some :: (Typeable a, Eq (f a)) => f a -> Any f
  Error :: String -> Any f

deriving instance (forall a. Show (Action state a)) => Show (Any (Action state))

instance Eq (Any f) where
  Some (a :: f a) == Some (b :: f b) =
    case eqT @a @b of
      Just Refl -> a == b
      Nothing -> False
  Error s == Error s' = s == s'
  _ == _ = False

data Step state where
  (:=) ::
    (Typeable a, Eq (Action state a), Show (Action state a)) =>
    Var a ->
    Action state a ->
    Step state

infix 5 :=

deriving instance (forall a. Show (Action state a)) => Show (Step state)

newtype Var a = Var Int
  deriving (Eq, Ord, Show, Typeable, Data)

instance Eq (Step state) where
  (Var i := act) == (Var j := act') =
    i == j && Some act == Some act'

-- Action sequences use Smart shrinking, but this is invisible to
-- client code because the extra Smart constructor is concealed by a
-- pattern synonym.

-- We also collect a list of names of actions which were generated,
-- but were then rejected by their precondition.

data Actions state = Actions_ [String] (Smart [Step state])

pattern Actions :: [Step state] -> Actions state
pattern Actions as <-
  Actions_ _ (Smart _ as)
  where
    Actions as = Actions_ [] (Smart 0 as)

{-# COMPLETE Actions #-}

instance Semigroup (Actions state) where
  Actions_ rs (Smart k as) <> Actions_ rs' (Smart _ as') = Actions_ (rs ++ rs') (Smart k (as <> as'))

instance Eq (Actions state) where
  Actions as == Actions as' = as == as'

instance (forall a. Show (Action state a)) => Show (Actions state) where
  showsPrec d (Actions as)
    | d > 10 = ("(" ++) . shows (Actions as) . (")" ++)
    | null as = ("Actions []" ++)
    | otherwise =
        ("Actions \n [" ++)
          . foldr
            (.)
            (shows (last as) . ("]" ++))
            [shows a . (",\n  " ++) | a <- init as]

instance (StateModel state) => Arbitrary (Actions state) where
  arbitrary = do
    (as, rejected) <- arbActions initialState 1
    return $ Actions_ rejected (Smart 0 as)
   where
    arbActions :: state -> Int -> Gen ([Step state], [String])
    arbActions s step = sized $ \n ->
      let w = n `div` 2 + 1
       in frequency
            [ (1, return ([], []))
            ,
              ( w
              , do
                  (mact, rej) <- satisfyPrecondition
                  case mact of
                    Just (Some act) -> do
                      (as, rejected) <- arbActions (nextState s act (Var step)) (step + 1)
                      return ((Var step := act) : as, rej ++ rejected)
                    Just Error{} -> error "impossible"
                    Nothing ->
                      return ([], [])
              )
            ]
     where
      satisfyPrecondition = sized $ \n -> go n (2 * n) [] -- idea copied from suchThatMaybe
      go m n rej
        | m > n = return (Nothing, rej)
        | otherwise = do
            a <- resize m $ arbitraryAction s
            case a of
              Some act ->
                if precondition s act
                  then return (Just (Some act), rej)
                  else go (m + 1) n (actionName act : rej)
              Error _ ->
                go (m + 1) n rej

  shrink (Actions_ rs as) =
    map (Actions_ rs) (shrinkSmart (map (prune . map fst) . shrinkList shrinker . withStates) as)
   where
    shrinker (Var i := act, s) = [(Var i := act', s) | Some act' <- shrinkAction s act]

prune :: StateModel state => [Step state] -> [Step state]
prune = loop initialState
 where
  loop _s [] = []
  loop s ((var := act) : as)
    | precondition s act =
        (var := act) : loop (nextState s act var) as
    | otherwise =
        loop s as

withStates :: StateModel state => [Step state] -> [(Step state, state)]
withStates = loop initialState
 where
  loop _s [] = []
  loop s ((var := act) : as) =
    (var := act, s) : loop (nextState s act var) as

stateAfter :: StateModel state => Actions state -> state
stateAfter (Actions actions) = loop initialState actions
 where
  loop s [] = s
  loop s ((var := act) : as) = loop (nextState s act var) as

runActions ::
  forall state m.
  (StateModel state, RunModel state m) =>
  Actions state ->
  PropertyM m (state, Env m)
runActions = runActionsInState @_ @m initialState

runActionsInState ::
  forall state m.
  (StateModel state, RunModel state m) =>
  state ->
  Actions state ->
  PropertyM m (state, Env m)
runActionsInState st (Actions_ rejected (Smart _ actions)) = loop st [] actions
 where
  loop _s env [] = do
    unless (null rejected) $
      monitor (tabulate "Actions rejected by precondition" rejected)
    return (_s, reverse env)
  loop s env ((Var n := act) : as) = do
    pre $ precondition s act
    ret <- run (perform s act (lookUpVar env))
    let name = actionName act
    monitor (tabulate "Actions" [name])
    let var = Var n
        s' = nextState s act var
        env' = (var :== ret) : env
    monitor (monitoring @state @m (s, s') act (lookUpVar env') ret)
    b <- run $ postcondition (s, s') act (lookUpVar env) ret
    assert b
    loop s' env' as

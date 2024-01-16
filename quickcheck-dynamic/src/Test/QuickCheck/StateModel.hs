{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Simple (stateful) Model-Based Testing library for use with Haskell QuickCheck.
--
-- This module provides the basic machinery to define a `StateModel` from which /traces/ can
-- be generated and executed against some /actual/ implementation code to define monadic `Property`
-- to be asserted by QuickCheck.
module Test.QuickCheck.StateModel (
  module Test.QuickCheck.StateModel.Variables,
  StateModel (..),
  RunModel (..),
  PostconditionM (..),
  WithUsedVars (..),
  Annotated (..),
  Step (..),
  Polarity (..),
  ActionWithPolarity (..),
  LookUp,
  Actions (..),
  pattern Actions,
  EnvEntry (..),
  pattern (:=?),
  Env,
  Realized,
  Generic,
  monitorPost,
  counterexamplePost,
  stateAfter,
  runActions,
  lookUpVar,
  lookUpVarMaybe,
  initialAnnotatedState,
  computeNextState,
  computePrecondition,
  computeArbitraryAction,
  computeShrinkAction,
  failureResult,
) where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.Data
import Data.Kind
import Data.List
import Data.Monoid (Endo (..))
import Data.Set qualified as Set
import GHC.Generics
import GHC.Stack
import Test.QuickCheck as QC
import Test.QuickCheck.DynamicLogic.SmartShrinking
import Test.QuickCheck.Monadic
import Test.QuickCheck.StateModel.Variables

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
--  * `validFailingAction`: Specifies when an action that fails it's `precondition` can still run as what is
--    called a _negative_ action. This means that the action is (1) expected to fail and (2) not expected to
--    change the model state. This is very useful for testing the checks and failure conditions in the SUT
--    are implemented correctly. Should it be necessary to update the model state with e.g. book-keeping for
--    a negative action one can define `failureNextState` - but it is generally recommended to let this be
--    as simple an action as possible.
class
  ( forall a. Show (Action state a)
  , forall a. HasVariables (Action state a)
  , Show state
  , HasVariables state
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
  arbitraryAction :: VarContext -> state -> Gen (Any (Action state))

  -- | Shrinker for `Action`.
  -- Defaults to no-op but as usual, defining a good shrinker greatly enhances the usefulness
  -- of property-based testing.
  shrinkAction :: Typeable a => VarContext -> state -> Action state a -> [Any (Action state)]
  shrinkAction _ _ _ = []

  -- | Initial state of generated traces.
  initialState :: state

  -- | Transition function for the model.
  -- The `Var a` parameter is useful to keep reference to actual value of type `a` produced
  -- by `perform`ing the `Action` inside the `state` so that further actions can use `Lookup`
  -- to retrieve that data. This allows the model to be ignorant of those values yet maintain
  -- some references that can be compared and looked for.
  nextState :: Typeable a => state -> Action state a -> Var a -> state
  nextState s _ _ = s

  -- | Transition function for negative actions. Note that most negative testing applications
  -- should not require an implementation of this function!
  failureNextState :: Typeable a => state -> Action state a -> state
  failureNextState s _ = s

  -- | Precondition for filtering generated `Action`.
  -- This function is applied before the action is performed, it is useful to refine generators that
  -- can produce more values than are useful.
  precondition :: state -> Action state a -> Bool
  precondition _ _ = True

  -- | Precondition for filtering an `Action` that can meaningfully run but is supposed to fail.
  -- An action will run as a _negative_ action if the `precondition` fails and `validFailingAction` succeeds.
  -- A negative action should have _no effect_ on the model state. This may not be desierable in all
  -- situations - in which case one can override this semantics for book-keeping in `failureNextState`.
  validFailingAction :: state -> Action state a -> Bool
  validFailingAction _ _ = False

deriving instance (forall a. Show (Action state a)) => Show (Any (Action state))

-- TODO: maybe it makes sense to write
-- out a long list of these instances
type family Realized (m :: Type -> Type) a :: Type
type instance Realized IO a = a
type instance Realized (StateT s m) a = Realized m a
type instance Realized (ReaderT r m) a = Realized m a
type instance Realized (WriterT w m) a = Realized m a
type instance Realized Identity a = a

newtype PostconditionM m a = PostconditionM {runPost :: WriterT (Endo Property, Endo Property) m a}
  deriving (Functor, Applicative, Monad)

instance MonadTrans PostconditionM where
  lift = PostconditionM . lift

-- | Apply the property transformation to the property after evaluating
-- the postcondition. Useful for collecting statistics while avoiding
-- duplication between `monitoring` and `postcondition`.
monitorPost :: Monad m => (Property -> Property) -> PostconditionM m ()
monitorPost m = PostconditionM $ tell (Endo m, mempty)

-- | Acts as `Test.QuickCheck.counterexample` if the postcondition fails.
counterexamplePost :: Monad m => String -> PostconditionM m ()
counterexamplePost c = PostconditionM $ tell (mempty, Endo $ counterexample c)

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
  postcondition :: forall a. (state, state) -> Action state a -> LookUp m -> Realized m a -> PostconditionM m Bool
  postcondition _ _ _ _ = pure True

  -- | Postcondition on the result of running a _negative_ `Action`.
  -- The result is `assert`ed and will make the property fail should it be `False`. This is useful
  -- to check the implementation produces e.g. the expected errors or to check that the SUT hasn't
  -- been updated during the execution of the negative action.
  postconditionOnFailure :: forall a. (state, state) -> Action state a -> LookUp m -> Realized m a -> PostconditionM m Bool
  postconditionOnFailure _ _ _ _ = pure True

  -- | Allows the user to attach additional information to the `Property` at each step of the process.
  -- This function is given the full transition that's been executed, including the start and ending
  -- `state`, the `Action`, the current environment to `Lookup` and the value produced by `perform`
  -- while executing this step.
  monitoring :: forall a. (state, state) -> Action state a -> LookUp m -> Realized m a -> Property -> Property
  monitoring _ _ _ _ prop = prop

-- | Indicate that the result of an action (in `perform`)
-- should not be inspected by the postcondition or appear
-- in a positive test. Useful when we want to give a type
-- for an `Action` like `SomeAct :: Action SomeState SomeType`
-- instead of `SomeAct :: Action SomeState (Either SomeError SomeType)`
-- but still need to return something in `perform` in the failure case.
failureResult :: HasCallStack => a
failureResult = error "A result of a failing action has been erronesouly inspected"

computePostcondition :: forall m state a. RunModel state m => (state, state) -> ActionWithPolarity state a -> LookUp m -> Realized m a -> PostconditionM m Bool
computePostcondition _ Pure{} _ _ = pure True
computePostcondition ss (ActionWithPolarity a p) l r
  | p == PosPolarity = postcondition ss a l r
  | otherwise = postconditionOnFailure ss a l r

type LookUp m = forall a. Typeable a => Var a -> Realized m a

type Env m = [EnvEntry m]

data EnvEntry m where
  (:==) :: Typeable a => Var a -> Realized m a -> EnvEntry m

infix 5 :==

pattern (:=?) :: forall a m. Typeable a => Var a -> Realized m a -> EnvEntry m
pattern v :=? val <- (viewAtType -> Just (v, val))

viewAtType :: forall a m. Typeable a => EnvEntry m -> Maybe (Var a, Realized m a)
viewAtType ((v :: Var b) :== val)
  | Just Refl <- eqT @a @b = Just (v, val)
  | otherwise = Nothing

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

data WithUsedVars a = WithUsedVars VarContext a

data Polarity
  = PosPolarity
  | NegPolarity
  deriving (Ord, Eq)

instance Show Polarity where
  show PosPolarity = "+"
  show NegPolarity = "-"

data ActionWithPolarity state a where
  ActionWithPolarity
    :: Eq (Action state a)
    => Action state a
    -> Polarity
    -> ActionWithPolarity state a
  Pure :: (Eq (Action state a), Typeable a) => Var a -> ActionWithPolarity state a

instance HasVariables (Action state a) => HasVariables (ActionWithPolarity state a) where
  getAllVariables (Pure v) = Set.singleton (Some v)
  getAllVariables (ActionWithPolarity a _) = getAllVariables a

deriving instance Eq (Action state a) => Eq (ActionWithPolarity state a)

data Step state where
  (:=)
    :: (Typeable a, Eq (Action state a), Show (Action state a))
    => Var a
    -> ActionWithPolarity state a
    -> Step state

infix 5 :=

instance (forall a. HasVariables (Action state a)) => HasVariables (Step state) where
  getAllVariables (var := act) = Set.insert (Some var) $ getAllVariables act

funName :: Polarity -> String
funName PosPolarity = "action"
funName _ = "failingAction"

polarity :: ActionWithPolarity state a -> Polarity
polarity (ActionWithPolarity _ p) = p
polarity Pure{} = PosPolarity

instance Show (Step state) where
  show (var := act) = show var ++ " <- " ++ show act

instance Show (Action state a) => Show (ActionWithPolarity state a) where
  show (ActionWithPolarity act p) = funName p ++ " $ " ++ show act
  show (Pure v) = "pure " ++ show v

instance Show (WithUsedVars (Step state)) where
  show (WithUsedVars ctx (var := act)) =
    if isWellTyped var ctx
      then show (var := act)
      else show act

instance Eq (Step state) where
  (v := act) == (v' := act') =
    unsafeCoerceVar v == v' && Some act == Some act'

-- Action sequences use Smart shrinking, but this is invisible to
-- client code because the extra Smart constructor is concealed by a
-- pattern synonym.

-- We also collect a list of names of actions which were generated,
-- but were then rejected by their precondition.

data Actions state = Actions_ [String] (Smart [Step state])
  deriving (Generic)

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

instance StateModel state => Show (Actions state) where
  show (Actions as) =
    let as' = WithUsedVars (usedVariables (Actions as)) <$> as
     in intercalate "\n" $ zipWith (++) ("do " : repeat "   ") (map show as' ++ ["pure ()"])

usedVariables :: forall state. StateModel state => Actions state -> VarContext
usedVariables (Actions as) = go initialAnnotatedState as
  where
    go :: Annotated state -> [Step state] -> VarContext
    go aState [] = allVariables (underlyingState aState)
    go aState ((var := act) : steps) =
      allVariables act
        <> allVariables (underlyingState aState)
        <> go (computeNextState aState act var) steps

instance forall state. StateModel state => Arbitrary (Actions state) where
  arbitrary = do
    (as, rejected) <- arbActions initialAnnotatedState 1
    return $ Actions_ rejected (Smart 0 as)
    where
      arbActions :: Annotated state -> Int -> Gen ([Step state], [String])
      arbActions s step = sized $ \n ->
        let w = n `div` 2 + 1
         in frequency
              [ (1, return ([], []))
              ,
                ( w
                , do
                    (mact, rej) <- satisfyPrecondition
                    case mact of
                      Just (Some act@ActionWithPolarity{}) -> do
                        let var = mkVar step
                        (as, rejected) <- arbActions (computeNextState s act var) (step + 1)
                        return ((var := act) : as, rej ++ rejected)
                      _ ->
                        return ([], [])
                )
              ]
        where
          satisfyPrecondition = sized $ \n -> go n (2 * n) [] -- idea copied from suchThatMaybe
          go m n rej
            | m > n = return (Nothing, rej)
            | otherwise = do
                a <- resize m $ computeArbitraryAction s
                case a of
                  Some act@(ActionWithPolarity act' _) ->
                    if computePrecondition s act
                      then return (Just (Some act), rej)
                      else go (m + 1) n (actionName act' : rej)
                  _ -> error "The impossible happened in arbitrary actions"

  shrink (Actions_ rs as) =
    map (Actions_ rs) (shrinkSmart (map (prune . map fst) . concatMap customActionsShrinker . shrinkList shrinker . withStates) as)
    where
      shrinker :: (Step state, Annotated state) -> [(Step state, Annotated state)]
      shrinker (v := act, s) =
        [ (step, s)
        | Some act' <- computeShrinkAction s act
        , let step = case act' of
                ActionWithPolarity{} -> unsafeCoerceVar v := act'
                Pure{} -> unsafeCoerceVar v := act'
        ]

      customActionsShrinker :: [(Step state, Annotated state)] -> [[(Step state, Annotated state)]]
      customActionsShrinker acts =
        let usedVars = mconcat [getAllVariables a <> getAllVariables (underlyingState s) | (_ := a, s) <- acts]
            binding (v := _, _) = Some v `Set.member` usedVars
            -- Remove at most one non-binding action
            go [] = [[]]
            go (p : ps)
              | binding p = map (p :) (go ps)
              | otherwise = ps : map (p :) (go ps)
         in go acts

-- Running state models

data Annotated state = Metadata
  { vars :: VarContext
  , underlyingState :: state
  }

instance Show state => Show (Annotated state) where
  show (Metadata ctx s) = show ctx ++ " |- " ++ show s

initialAnnotatedState :: StateModel state => Annotated state
initialAnnotatedState = Metadata mempty initialState

actionWithPolarity :: (StateModel state, Eq (Action state a)) => Annotated state -> Action state a -> ActionWithPolarity state a
actionWithPolarity s a =
  let p
        | precondition (underlyingState s) a = PosPolarity
        | validFailingAction (underlyingState s) a = NegPolarity
        | otherwise = PosPolarity
   in ActionWithPolarity a p

computePrecondition :: StateModel state => Annotated state -> ActionWithPolarity state a -> Bool
computePrecondition s (Pure v) = v `isWellTyped` vars s
computePrecondition s (ActionWithPolarity a p) =
  let polarPrecondition
        | p == PosPolarity = precondition (underlyingState s) a
        | otherwise = validFailingAction (underlyingState s) a && not (precondition (underlyingState s) a)
   in all (\(Some v) -> v `isWellTyped` vars s) (getAllVariables a)
        && polarPrecondition

computeNextState
  :: (StateModel state, Typeable a)
  => Annotated state
  -> ActionWithPolarity state a
  -> Var a
  -> Annotated state
computeNextState s Pure{} v = Metadata (extendContext (vars s) v) (underlyingState s)
computeNextState s (ActionWithPolarity a p) v
  | p == PosPolarity = Metadata (extendContext (vars s) v) (nextState (underlyingState s) a v)
  | otherwise = Metadata (vars s) (failureNextState (underlyingState s) a)

computeArbitraryAction
  :: StateModel state
  => Annotated state
  -> Gen (Any (ActionWithPolarity state))
computeArbitraryAction s = do
  Some a <- arbitraryAction (vars s) (underlyingState s)
  pure $ Some $ actionWithPolarity s a

computeShrinkAction
  :: forall state a
   . (Typeable a, StateModel state)
  => Annotated state
  -> ActionWithPolarity state a
  -> [Any (ActionWithPolarity state)]
computeShrinkAction s (Pure v) =
  [Some (Pure v') | v' <- shrinkVar (vars s) v]
computeShrinkAction s (ActionWithPolarity a _) =
  [Some (actionWithPolarity s a') | Some a' <- shrinkAction (vars s) (underlyingState s) a]
    ++ [ Some (Pure (v' :: Var a))
       | Some v <- Set.toList $ getAllVariables a
       , Just v' <- [cast v]
       ]

prune :: forall state. StateModel state => [Step state] -> [Step state]
prune = loop initialAnnotatedState
  where
    loop _s [] = []
    loop s ((var := act) : as)
      | computePrecondition @state s act =
          (var := act) : loop (computeNextState s act var) as
      | otherwise =
          loop s as

withStates :: forall state. StateModel state => [Step state] -> [(Step state, Annotated state)]
withStates = loop initialAnnotatedState
  where
    loop _s [] = []
    loop s ((var := act) : as) =
      (var := act, s) : loop (computeNextState @state s act var) as

stateAfter :: forall state. StateModel state => Actions state -> Annotated state
stateAfter (Actions actions) = loop initialAnnotatedState actions
  where
    loop s [] = s
    loop s ((var := act) : as) = loop (computeNextState @state s act var) as

runActions
  :: forall state m
   . (StateModel state, RunModel state m)
  => Actions state
  -> PropertyM m (Annotated state, Env m)
runActions (Actions_ rejected (Smart _ actions)) = loop initialAnnotatedState [] actions
  where
    loop :: Annotated state -> Env m -> [Step state] -> PropertyM m (Annotated state, Env m)
    loop _s env [] = do
      unless (null rejected) $
        monitor $
          tabulate "Actions rejected by precondition" rejected
      return (_s, reverse env)
    loop s env ((v := act) : as) = do
      pre $ computePrecondition s act
      ret <- run $ case act of
        ActionWithPolarity act' _ -> perform (underlyingState s) act' (lookUpVar env)
        Pure var -> pure $ lookUpVar env var
      let name
            | Pure{} <- act = "pure"
            | ActionWithPolarity act' p <- act = show p ++ actionName act'
      monitor $ tabulate "Actions" [name]
      let var = unsafeCoerceVar v
          s' = computeNextState s act var
          env' = (var :== ret) : env
      monitor $ tabulate "Action polarity" [show $ polarity act]
      case act of
        Pure{} -> pure ()
        ActionWithPolarity act' _ -> monitor $ monitoring @state @m (underlyingState s, underlyingState s') act' (lookUpVar env') ret
      (b, (Endo mon, Endo onFail)) <-
        run
          . runWriterT
          . runPost
          $ computePostcondition @m
            (underlyingState s, underlyingState s')
            act
            (lookUpVar env)
            ret
      monitor mon
      unless b $ monitor onFail
      assert b
      loop s' env' as

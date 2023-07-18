module Test.QuickCheck.DynamicLogic.Internal where

import Control.Applicative
import Control.Arrow (second)
import Control.Monad
import Data.Typeable
import Test.QuickCheck hiding (generate)
import Test.QuickCheck.DynamicLogic.CanGenerate
import Test.QuickCheck.DynamicLogic.Quantify
import Test.QuickCheck.DynamicLogic.SmartShrinking
import Test.QuickCheck.DynamicLogic.Utils qualified as QC
import Test.QuickCheck.StateModel

-- | A `DynFormula` may depend on the QuickCheck size parameter
newtype DynFormula s = DynFormula {unDynFormula :: Int -> DynLogic s}

-- | Base Dynamic logic formulae language.
-- Formulae are parameterised
-- over the type of state `s` to which they apply. A `DynLogic` value
-- cannot be constructed directly, one has to use the various "smart
-- constructors" provided, see the /Building formulae/ section.
data DynLogic s
  = -- | False
    EmptySpec
  | -- | True
    Stop
  | -- | After any action the predicate should hold
    AfterAny (DynPred s)
  | -- | Choice (angelic or demonic)
    Alt ChoiceType (DynLogic s) (DynLogic s)
  | -- | Prefer this branch if trying to stop.
    Stopping (DynLogic s)
  | -- | After a specific action the predicate should hold
    forall a.
    (Eq (Action s a), Show (Action s a), Typeable a) =>
    After (ActionWithPolarity s a) (Var a -> DynPred s)
  | Error String (DynPred s)
  | -- | Adjust the probability of picking a branch
    Weight Double (DynLogic s)
  | -- | Generating a random value
    forall a.
    QuantifyConstraints a =>
    ForAll (Quantification a) (a -> DynLogic s)
  | -- | Apply a QuickCheck property modifier (like `tabulate` or `collect`)
    Monitor (Property -> Property) (DynLogic s)

data ChoiceType = Angelic | Demonic
  deriving (Eq, Show)

type DynPred s = Annotated s -> DynLogic s

-- * Building formulae

-- | Ignore this formula, i.e. backtrack and try something else. @forAllScripts ignore (const True)@
--   will discard all test cases (equivalent to @False ==> True@).
ignore :: DynFormula s
ignore = DynFormula . const $ EmptySpec

-- | `True` for DL formulae.
passTest :: DynFormula s
passTest = DynFormula . const $ Stop

-- | Given `f` must be `True` given /any/ state.
afterAny :: (Annotated s -> DynFormula s) -> DynFormula s
afterAny f = DynFormula $ \n -> AfterAny $ \s -> unDynFormula (f s) n

afterPolar
  :: (Typeable a, Eq (Action s a), Show (Action s a))
  => ActionWithPolarity s a
  -> (Var a -> Annotated s -> DynFormula s)
  -> DynFormula s
afterPolar act f = DynFormula $ \n -> After act $ \x s -> unDynFormula (f x s) n

-- | Given `f` must be `True` after /some/ action.
-- `f` is passed the state resulting from executing the `Action`.
after
  :: (Typeable a, Eq (Action s a), Show (Action s a))
  => Action s a
  -> (Var a -> Annotated s -> DynFormula s)
  -> DynFormula s
after act f = afterPolar (ActionWithPolarity act PosPolarity) f

-- | Given `f` must be `True` after /some/ negative action.
-- `f` is passed the state resulting from executing the `Action`
-- as a negative action.
afterNegative
  :: (Typeable a, Eq (Action s a), Show (Action s a))
  => Action s a
  -> (Annotated s -> DynFormula s)
  -> DynFormula s
afterNegative act f = afterPolar (ActionWithPolarity act NegPolarity) (const f)

-- | Disjunction for DL formulae.
-- Is `True` if either formula is `True`. The choice is /angelic/, ie. it is
-- always made by the "caller". This is  mostly important in case a test is
-- `Stuck`.
(|||) :: DynFormula s -> DynFormula s -> DynFormula s
-- In formulae, we use only angelic choice. But it becomes demonic
-- after one step (that is, the choice has been made).
DynFormula f ||| DynFormula g = DynFormula $ \n -> Alt Angelic (f n) (g n)

-- | First-order quantification of variables.
-- Formula @f@ is `True` iff. it is `True` /for all/ possible values of `q`. The
-- underlying framework will generate values of `q` and check the formula holds
-- for those values. `Quantifiable` values are thus values that can be generated
-- and checked and the `Test.QuickCheck.DynamicLogic.Quantify` module defines
-- basic combinators to build those from building blocks.
forAllQ
  :: Quantifiable q
  => q
  -> (Quantifies q -> DynFormula s)
  -> DynFormula s
forAllQ q f
  | isEmptyQ q' = ignore
  | otherwise = DynFormula $ \n -> ForAll q' $ ($ n) . unDynFormula . f
  where
    q' = quantify q

-- | Adjust weight for selecting formula.
-- This is mostly useful in relation with `(|||)` combinator, in order to tweak the
-- priority for generating the next step(s) of the test that matches the formula.
weight :: Double -> DynFormula s -> DynFormula s
weight w f = DynFormula $ Weight w . unDynFormula f

-- | Get the current QuickCheck size parameter.
withSize :: (Int -> DynFormula s) -> DynFormula s
withSize f = DynFormula $ \n -> unDynFormula (f n) n

-- | Prioritise doing this if we are
-- trying to stop generation.
toStop :: DynFormula s -> DynFormula s
toStop (DynFormula f) = DynFormula $ Stopping . f

-- | Successfully ends the test.
done :: Annotated s -> DynFormula s
done _ = passTest

-- | Ends test with given error message.
errorDL :: String -> DynFormula s
errorDL s = DynFormula . const $ Error s (const EmptySpec)

-- | Embed QuickCheck's monitoring functions (eg. `label`, `tabulate`) in
-- a formula.
-- This is useful to improve the reporting from test execution, esp. in the
-- case of failures.
monitorDL :: (Property -> Property) -> DynFormula s -> DynFormula s
monitorDL m (DynFormula f) = DynFormula $ Monitor m . f

-- | Formula should hold at any state.
-- In effect this leads to exploring alternatives from a given state `s` and ensuring
-- formula holds in all those states.
always :: (Annotated s -> DynFormula s) -> (Annotated s -> DynFormula s)
always p s = withSize $ \n -> toStop (p s) ||| p s ||| weight (fromIntegral n) (afterAny (always p))

data FailingAction s
  = ErrorFail String
  | forall a. (Typeable a, Eq (Action s a)) => ActionFail (ActionWithPolarity s a)

instance StateModel s => HasVariables (FailingAction s) where
  getAllVariables ErrorFail{} = mempty
  getAllVariables (ActionFail a) = getAllVariables a

instance StateModel s => Eq (FailingAction s) where
  ErrorFail s == ErrorFail s' = s == s'
  ActionFail (a :: ActionWithPolarity s a) == ActionFail (a' :: ActionWithPolarity s a')
    | Just Refl <- eqT @a @a' = a == a'
  _ == _ = False

instance StateModel s => Show (FailingAction s) where
  show (ErrorFail s) = "Error " ++ show s
  show (ActionFail (ActionWithPolarity a pol)) = show pol ++ " : " ++ show a

data DynLogicTest s
  = BadPrecondition (TestSequence s) (FailingAction s) (Annotated s)
  | Looping (TestSequence s)
  | Stuck (TestSequence s) (Annotated s)
  | DLScript (TestSequence s)

data Witnesses r where
  Do :: r -> Witnesses r
  Witness :: QuantifyConstraints a => a -> Witnesses r -> Witnesses r

discardWitnesses :: Witnesses r -> r
discardWitnesses (Do r) = r
discardWitnesses (Witness _ k) = discardWitnesses k

pattern Witnesses :: Witnesses () -> r -> Witnesses r
pattern Witnesses w r <- ((\wit -> (void wit, discardWitnesses wit)) -> (w, r))
  where
    Witnesses w r = r <$ w

{-# COMPLETE Witnesses #-}

deriving instance Functor Witnesses
deriving instance Foldable Witnesses
deriving instance Traversable Witnesses

instance Eq r => Eq (Witnesses r) where
  Do r == Do r' = r == r'
  Witness (a :: a) k == Witness (a' :: a') k' =
    case eqT @a @a' of
      Just Refl -> a == a' && k == k'
      Nothing -> False
  _ == _ = False

instance Show r => Show (Witnesses r) where
  show (Do r) = "Do $ " ++ show r
  show (Witness a k) = "Witness (" ++ show a ++ " :: " ++ show (typeOf a) ++ ")\n" ++ show k -- TODO

type TestStep s = Witnesses (Step s)

newtype TestSequence s = TestSeq (Witnesses (TestContinuation s))
  deriving (Show, Eq)

data TestContinuation s
  = ContStep (Step s) (TestSequence s)
  | ContStop

pattern TestSeqStop :: TestSequence s
pattern TestSeqStop = TestSeq (Do ContStop)

pattern TestSeqStep :: Step s -> TestSequence s -> TestSequence s
pattern TestSeqStep s ss = TestSeq (Do (ContStep s ss))

-- The `()` are the constraints required to use the pattern, and the `(Typeable a, ...)` are the
-- ones provided when you do (including a fresh type variable `a`).
-- See https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/pattern_synonyms.html#typing-of-pattern-synonyms
pattern TestSeqWitness :: () => forall a. QuantifyConstraints a => a -> TestSequence s -> TestSequence s
pattern TestSeqWitness a ss <- TestSeq (Witness a (TestSeq -> ss))
  where
    TestSeqWitness a (TestSeq ss) = TestSeq (Witness a ss)

{-# COMPLETE TestSeqWitness, TestSeqStep, TestSeqStop #-}

deriving instance StateModel s => Show (TestContinuation s)
deriving instance StateModel s => Eq (TestContinuation s)

consSeq :: TestStep s -> TestSequence s -> TestSequence s
consSeq step ss = TestSeq $ flip ContStep ss <$> step

unconsSeq :: TestSequence s -> Maybe (TestStep s, TestSequence s)
unconsSeq (TestSeq ss) =
  case discardWitnesses ss of
    ContStop -> Nothing
    ContStep s rest -> Just (s <$ ss, rest)

unstopSeq :: TestSequence s -> Maybe (Witnesses ())
unstopSeq (TestSeq ss) =
  case discardWitnesses ss of
    ContStop -> Just $ () <$ ss
    ContStep{} -> Nothing

pattern TestSeqStopW :: Witnesses () -> TestSequence s
pattern TestSeqStopW w <- (unstopSeq -> Just w)
  where
    TestSeqStopW w = TestSeq (ContStop <$ w)

pattern (:>) :: TestStep s -> TestSequence s -> TestSequence s
pattern step :> ss <- (unconsSeq -> Just (step, ss))
  where
    step :> ss = consSeq step ss

{-# COMPLETE TestSeqStopW, (:>) #-}

nullSeq :: TestSequence s -> Bool
nullSeq TestSeqStop = True
nullSeq _ = False

dropSeq :: Int -> TestSequence s -> TestSequence s
dropSeq n _ | n < 0 = error "dropSeq: negative number"
dropSeq 0 ss = ss
dropSeq _ TestSeqStop = TestSeqStop
dropSeq n (TestSeqWitness _ ss) = dropSeq (n - 1) ss
dropSeq n (TestSeqStep _ ss) = dropSeq (n - 1) ss

getContinuation :: TestSequence s -> TestSequence s
getContinuation (TestSeq w) = case discardWitnesses w of
  ContStop -> TestSeqStop
  ContStep _ s -> s

unlines' :: [String] -> String
unlines' [] = ""
unlines' xs = init $ unlines xs

prettyTestSequence :: VarContext -> TestSequence s -> String
prettyTestSequence ctx ss = unlines' $ zipWith (++) ("do " : repeat "   ") $ prettySeq ss
  where
    prettySeq (TestSeqStopW w) = prettyWitnesses w
    prettySeq (Witnesses w step :> ss') = prettyWitnesses w ++ show (WithUsedVars ctx step) : prettySeq ss'

prettyWitnesses :: Witnesses () -> [String]
prettyWitnesses (Witness a w) = ("_ <- forAllQ $ exactlyQ $ " ++ show a) : prettyWitnesses w
prettyWitnesses Do{} = []

instance StateModel s => Show (DynLogicTest s) where
  show (BadPrecondition ss bad s) =
    prettyTestSequence (usedVariables ss <> allVariables bad) ss
      ++ "\n   -- In state: "
      ++ show s
      ++ "\n   "
      ++ prettyBad bad
    where
      prettyBad :: FailingAction s -> String
      prettyBad (ErrorFail e) = "assert " ++ show e ++ " False"
      prettyBad (ActionFail (ActionWithPolarity a p)) = f ++ " $ " ++ show a ++ "  -- Failed precondition\n   pure ()"
        where
          f
            | p == PosPolarity = "action"
            | otherwise = "negativeAction"
  show (Looping ss) = prettyTestSequence (usedVariables ss) ss ++ "\n   pure ()\n   -- Looping"
  show (Stuck ss s) = prettyTestSequence (usedVariables ss) ss ++ "\n   pure ()\n   -- Stuck in state " ++ show s
  show (DLScript ss) = prettyTestSequence (usedVariables ss) ss ++ "\n   pure ()\n"

usedVariables :: forall s. StateModel s => TestSequence s -> VarContext
usedVariables = go initialAnnotatedState
  where
    go :: Annotated s -> TestSequence s -> VarContext
    go aState TestSeqStop = allVariables (underlyingState aState)
    go aState (TestSeqWitness a ss) = allVariables a <> go aState ss
    go aState (TestSeqStep step@(_ := act) ss) =
      allVariables act
        <> allVariables (underlyingState aState)
        <> go (nextStateStep step aState) ss

-- | Restricted calls are not generated by "AfterAny"; they are included
-- in tests explicitly using "After" in order to check specific
-- properties at controlled times, so they are likely to fail if
-- invoked at other times.
class StateModel s => DynLogicModel s where
  restricted :: Action s a -> Bool
  restricted _ = False

restrictedPolar :: DynLogicModel s => ActionWithPolarity s a -> Bool
restrictedPolar (ActionWithPolarity a _) = restricted a

-- * Generate Properties

-- | Simplest "execution" function for `DynFormula`.
-- Turns a given a `DynFormula` paired with an interpreter function to produce some result from an

--- `Actions` sequence into a proper `Property` than can then be run by QuickCheck.
forAllScripts
  :: (DynLogicModel s, Testable a)
  => DynFormula s
  -> (Actions s -> a)
  -> Property
forAllScripts = forAllMappedScripts id id

-- | `Property` function suitable for formulae without choice.
forAllUniqueScripts
  :: (DynLogicModel s, Testable a)
  => Annotated s
  -> DynFormula s
  -> (Actions s -> a)
  -> Property
forAllUniqueScripts s f k =
  QC.withSize $ \sz ->
    let d = unDynFormula f sz
        n = unsafeNextVarIndex $ vars s
     in case generate chooseUniqueNextStep d n s 500 of
          Nothing -> counterexample "Generating Non-unique script in forAllUniqueScripts" False
          Just test -> validDLTest d test . applyMonitoring d test . property $ k (scriptFromDL test)

-- | Creates a `Property` from `DynFormula` with some specialised isomorphism for shrinking purpose.
forAllMappedScripts
  :: (DynLogicModel s, Testable a)
  => (rep -> DynLogicTest s)
  -> (DynLogicTest s -> rep)
  -> DynFormula s
  -> (Actions s -> a)
  -> Property
forAllMappedScripts to from f k =
  QC.withSize $ \n ->
    let d = unDynFormula f n
     in forAllShrinkBlind
          (Smart 0 <$> sized ((from <$>) . generateDLTest d))
          (shrinkSmart ((from <$>) . shrinkDLTest d . to))
          $ \(Smart _ script) ->
            withDLScript d k (to script)

withDLScript :: (DynLogicModel s, Testable a) => DynLogic s -> (Actions s -> a) -> DynLogicTest s -> Property
withDLScript d k test =
  validDLTest d test . applyMonitoring d test . property $ k (scriptFromDL test)

withDLScriptPrefix :: (DynLogicModel s, Testable a) => DynFormula s -> (Actions s -> a) -> DynLogicTest s -> Property
withDLScriptPrefix f k test =
  QC.withSize $ \n ->
    let d = unDynFormula f n
        test' = unfailDLTest d test
     in validDLTest d test' . applyMonitoring d test' . property $ k (scriptFromDL test')

generateDLTest :: DynLogicModel s => DynLogic s -> Int -> Gen (DynLogicTest s)
generateDLTest d size = generate chooseNextStep d 0 (initialStateFor d) size

onDLTestSeq :: (TestSequence s -> TestSequence s) -> DynLogicTest s -> DynLogicTest s
onDLTestSeq f (BadPrecondition ss bad s) = BadPrecondition (f ss) bad s
onDLTestSeq f (Looping ss) = Looping (f ss)
onDLTestSeq f (Stuck ss s) = Stuck (f ss) s
onDLTestSeq f (DLScript ss) = DLScript (f ss)

consDLTest :: TestStep s -> DynLogicTest s -> DynLogicTest s
consDLTest step = onDLTestSeq (step :>)

consDLTestW :: Witnesses () -> DynLogicTest s -> DynLogicTest s
consDLTestW w = onDLTestSeq (addW w)
  where
    addW Do{} ss = ss
    addW (Witness a w') ss = TestSeqWitness a (addW w' ss)

generate
  :: (Monad m, DynLogicModel s)
  => (Annotated s -> Int -> DynLogic s -> m (NextStep s))
  -> DynLogic s
  -> Int
  -> Annotated s
  -> Int
  -> m (DynLogicTest s)
generate chooseNextStepFun d n s size =
  if n > sizeLimit size
    then return $ Looping TestSeqStop
    else do
      let preferred = if n > size then stopping d else noStopping d
          useStep (BadAction (Witnesses ws bad)) _ = return $ BadPrecondition (TestSeqStopW ws) bad s
          useStep StoppingStep _ = return $ DLScript TestSeqStop
          useStep (Stepping step d') _ =
            case discardWitnesses step of
              var := act ->
                consDLTest step
                  <$> generate
                    chooseNextStepFun
                    d'
                    (n + 1)
                    (computeNextState s act var)
                    size
          useStep NoStep alt = alt
      foldr
        (\step k -> do try <- chooseNextStepFun s n step; useStep try k)
        (return $ Stuck TestSeqStop s)
        [preferred, noAny preferred, d, noAny d]

sizeLimit :: Int -> Int
sizeLimit size = 2 * size + 20

initialStateFor :: StateModel s => DynLogic s -> Annotated s
initialStateFor _ = initialAnnotatedState

stopping :: DynLogic s -> DynLogic s
stopping EmptySpec = EmptySpec
stopping Stop = Stop
stopping (After act k) = After act k
stopping (Error m k) = Error m k
stopping (AfterAny _) = EmptySpec
stopping (Alt b d d') = Alt b (stopping d) (stopping d')
stopping (Stopping d) = d
stopping (Weight w d) = Weight w (stopping d)
stopping (ForAll _ _) = EmptySpec -- ???
stopping (Monitor f d) = Monitor f (stopping d)

noStopping :: DynLogic s -> DynLogic s
noStopping EmptySpec = EmptySpec
noStopping Stop = EmptySpec
noStopping (After act k) = After act k
noStopping (Error m k) = Error m k
noStopping (AfterAny k) = AfterAny k
noStopping (Alt b d d') = Alt b (noStopping d) (noStopping d')
noStopping (Stopping _) = EmptySpec
noStopping (Weight w d) = Weight w (noStopping d)
noStopping (ForAll q f) = ForAll q f
noStopping (Monitor f d) = Monitor f (noStopping d)

noAny :: DynLogic s -> DynLogic s
noAny EmptySpec = EmptySpec
noAny Stop = Stop
noAny (After act k) = After act k
noAny (Error m k) = Error m k
noAny (AfterAny _) = EmptySpec
noAny (Alt b d d') = Alt b (noAny d) (noAny d')
noAny (Stopping d) = Stopping (noAny d)
noAny (Weight w d) = Weight w (noAny d)
noAny (ForAll q f) = ForAll q f
noAny (Monitor f d) = Monitor f (noAny d)

nextSteps :: DynLogic s -> Gen [(Double, Witnesses (DynLogic s))]
nextSteps = nextSteps' generateQ

nextSteps' :: Monad m => (forall a. Quantification a -> m a) -> DynLogic s -> m [(Double, Witnesses (DynLogic s))]
nextSteps' _ EmptySpec = pure []
nextSteps' _ Stop = pure [(1, Do $ Stop)]
nextSteps' _ (After act k) = pure [(1, Do $ After act k)]
nextSteps' _ (Error m k) = pure [(1, Do $ Error m k)]
nextSteps' _ (AfterAny k) = pure [(1, Do $ AfterAny k)]
nextSteps' gen (Alt _ d d') = (++) <$> nextSteps' gen d <*> nextSteps' gen d'
nextSteps' gen (Stopping d) = nextSteps' gen d
nextSteps' gen (Weight w d) = do
  steps <- nextSteps' gen d
  pure [(w * w', s) | (w', s) <- steps, w * w' > never]
nextSteps' gen (ForAll q f) = do
  x <- gen q
  map (second $ Witness x) <$> nextSteps' gen (f x)
nextSteps' gen (Monitor _f d) = nextSteps' gen d

chooseOneOf :: [(Double, a)] -> Gen a
chooseOneOf steps = frequency [(round (w / never), return s) | (w, s) <- steps]

never :: Double
never = 1.0e-9

data NextStep s
  = StoppingStep
  | Stepping (Witnesses (Step s)) (DynLogic s)
  | NoStep
  | BadAction (Witnesses (FailingAction s))

chooseNextStep :: DynLogicModel s => Annotated s -> Int -> DynLogic s -> Gen (NextStep s)
chooseNextStep s n d = do
  nextSteps d >>= \case
    [] -> return NoStep
    steps -> do
      let bads = concatMap (findBad . snd) steps
          findBad = traverse $ flip badActions s
      case bads of
        [] -> do
          chosen <- chooseOneOf steps
          let takeStep = \case
                Stop -> return StoppingStep
                After a k ->
                  return $ Stepping (Do $ mkVar n := a) (k (mkVar n) (computeNextState s a (mkVar n)))
                AfterAny k -> do
                  m <- keepTryingUntil 100 (computeArbitraryAction s) $
                    \case
                      Some act -> computePrecondition s act && not (restrictedPolar act)
                  case m of
                    Nothing -> return NoStep
                    Just (Some a@ActionWithPolarity{}) ->
                      return $
                        Stepping
                          (Do $ mkVar n := a)
                          (k (computeNextState s a (mkVar n)))
                EmptySpec -> error "chooseNextStep: EmptySpec"
                ForAll{} -> error "chooseNextStep: ForAll"
                Error{} -> error "chooseNextStep: Error"
                Alt{} -> error "chooseNextStep: Alt"
                Stopping{} -> error "chooseNextStep: Stopping"
                Weight{} -> error "chooseNextStep: Weight"
                Monitor{} -> error "chooseNextStep: Monitor"
              go (Do d') = takeStep d'
              go (Witness a step) =
                go step
                  >>= pure . \case
                    StoppingStep -> StoppingStep -- TODO: This is a bit fishy
                    Stepping step' dl -> Stepping (Witness a step') dl
                    NoStep -> NoStep
                    BadAction bad -> BadAction (Witness a bad)
          go chosen
        b : _ -> return $ BadAction b

chooseUniqueNextStep :: (MonadFail m, DynLogicModel s) => Annotated s -> Int -> DynLogic s -> m (NextStep s)
chooseUniqueNextStep s n d = do
  steps <- map snd <$> nextSteps' (const bad) d
  case steps of
    [] -> return NoStep
    [Do EmptySpec] -> return NoStep
    [Do Stop] -> return StoppingStep
    [Do (After a k)] -> return $ Stepping (Do $ mkVar n := a) (k (mkVar n) (computeNextState s a (mkVar n)))
    _ -> bad
  where
    bad = fail "chooseUniqueNextStep: non-unique action in DynLogic"

keepTryingUntil :: Int -> Gen a -> (a -> Bool) -> Gen (Maybe a)
keepTryingUntil 0 _ _ = return Nothing
keepTryingUntil n g p = do
  x <- g
  if p x then return $ Just x else scale (+ 1) $ keepTryingUntil (n - 1) g p

shrinkDLTest :: DynLogicModel s => DynLogic s -> DynLogicTest s -> [DynLogicTest s]
shrinkDLTest _ (Looping _) = []
shrinkDLTest d tc =
  [ test | as' <- shrinkScript d (getScript tc), let pruned = pruneDLTest d as'
                                                     test = makeTestFromPruned d pruned,
  -- Don't shrink a non-executable test case to an executable one.
  case (tc, test) of
    (DLScript _, _) -> True
    (_, DLScript _) -> False
    _ -> True
  ]

nextStateStep :: StateModel s => Step s -> Annotated s -> Annotated s
nextStateStep (var := act) s = computeNextState s act var

shrinkScript :: DynLogicModel s => DynLogic s -> TestSequence s -> [TestSequence s]
shrinkScript = shrink' initialAnnotatedState
  where
    shrink' :: DynLogicModel s => Annotated s -> DynLogic s -> TestSequence s -> [TestSequence s]
    shrink' s d ss = structural s d ss ++ nonstructural s d ss

    nonstructural s d (TestSeqWitness a ss) =
      [ TestSeqWitness a' ss'
      | a' <- shrinkWitness d a
      , ss' <- ss : shrink' s (stepDLSeq d s $ TestSeqWitness a TestSeqStop) ss
      ]
    nonstructural s d (TestSeqStep step@(var := act) ss) =
      [TestSeqStep (unsafeCoerceVar var := act') ss | Some act'@ActionWithPolarity{} <- computeShrinkAction s act]
        ++ [ TestSeqStep step ss'
           | ss' <-
              shrink'
                (nextStateStep step s)
                (stepDLStep d s step)
                ss
           ]
    nonstructural _ _ TestSeqStop = []

    structural _ _ TestSeqStopW{} = []
    structural s d (step :> rest) =
      TestSeqStop
        : reverse (takeWhile (not . nullSeq) [dropSeq (n - 1) rest | n <- iterate (* 2) 1])
        ++ map (step :>) (shrink' (nextStateStep (discardWitnesses step) s) (stepDLSeq d s (step :> TestSeqStop)) rest)

shrinkWitness :: (StateModel s, Typeable a) => DynLogic s -> a -> [a]
shrinkWitness (ForAll (q :: Quantification a) _) (a :: a') =
  case eqT @a @a' of
    Just Refl | isaQ q a -> shrinkQ q a
    _ -> []
shrinkWitness (Alt _ d d') a = shrinkWitness d a ++ shrinkWitness d' a
shrinkWitness (Stopping d) a = shrinkWitness d a
shrinkWitness (Weight _ d) a = shrinkWitness d a
shrinkWitness (Monitor _ d) a = shrinkWitness d a
shrinkWitness EmptySpec{} _ = []
shrinkWitness Stop{} _ = []
shrinkWitness Error{} _ = []
shrinkWitness After{} _ = []
shrinkWitness AfterAny{} _ = []

-- The result of pruning a list of actions is a prefix of a list of actions that
-- could have been generated by the dynamic logic.
pruneDLTest :: DynLogicModel s => DynLogic s -> TestSequence s -> TestSequence s
pruneDLTest dl = prune [dl] initialAnnotatedState
  where
    prune [] _ _ = TestSeqStop
    prune _ _ TestSeqStop = TestSeqStop
    prune ds s (TestSeqWitness a ss) =
      case [d' | d <- ds, d' <- stepDLW d a] of
        [] -> prune ds s ss
        ds' -> TestSeqWitness a $ prune ds' s ss
    prune ds s (TestSeqStep step@(_ := act) ss)
      | computePrecondition s act =
          case [d' | d <- ds, d' <- stepDL d s (Do step)] of
            [] -> prune ds s ss
            ds' -> TestSeqStep step $ prune ds' (nextStateStep step s) ss
      | otherwise = prune ds s ss

stepDL :: DynLogicModel s => DynLogic s -> Annotated s -> TestStep s -> [DynLogic s]
stepDL (After a k) s (Do (var := act))
  -- TOOD: make this nicer when we migrate to 9.2 where we can just bind
  -- the type variables cleanly and do `Just Refl <- eqT ...` here instead.
  | Some a == Some act = [k (unsafeCoerceVar var) (computeNextState s act (unsafeCoerceVar var))]
stepDL (AfterAny k) s (Do (var := act))
  | not (restrictedPolar act) = [k (computeNextState s act var)]
stepDL (Alt _ d d') s step = stepDL d s step ++ stepDL d' s step
stepDL (Stopping d) s step = stepDL d s step
stepDL (Weight _ d) s step = stepDL d s step
stepDL (ForAll (q :: Quantification a) f) s (Witness (a :: a') step) =
  case eqT @a @a' of
    Just Refl -> [d | isaQ q a, d <- stepDL (f a) s step]
    Nothing -> []
stepDL (Monitor _f d) s step = stepDL d s step
stepDL _ _ _ = []

stepDLW :: forall a s. (DynLogicModel s, Typeable a) => DynLogic s -> a -> [DynLogic s]
stepDLW (ForAll (q :: Quantification a') k) a =
  case eqT @a @a' of
    Just Refl -> [k a | isaQ q a]
    Nothing -> []
stepDLW (Alt _ d d') a = stepDLW d a ++ stepDLW d' a
stepDLW (Monitor _ d) a = stepDLW d a
stepDLW (Weight _ d) a = stepDLW d a
stepDLW (Stopping d) a = stepDLW d a
stepDLW _ _ = []

stepDLSeq :: DynLogicModel s => DynLogic s -> Annotated s -> TestSequence s -> DynLogic s
stepDLSeq d _ (TestSeqStopW Do{}) = d
stepDLSeq d s (TestSeqStopW (Witness a w)) = stepDLSeq (stepDLWitness d a) s (TestSeqStopW w)
stepDLSeq d s (step@(Witnesses _ (var := act)) :> ss) =
  stepDLSeq (demonicAlt $ stepDL d s step) (computeNextState s act var) ss

stepDLWitness :: forall a s. (DynLogicModel s, Typeable a) => DynLogic s -> a -> DynLogic s
stepDLWitness d a = demonicAlt $ stepDLW d a

stepDLStep :: DynLogicModel s => DynLogic s -> Annotated s -> Step s -> DynLogic s
stepDLStep d s step = stepDLSeq d s (TestSeqStep step TestSeqStop)

demonicAlt :: [DynLogic s] -> DynLogic s
demonicAlt [] = EmptySpec
demonicAlt ds = foldr1 (Alt Demonic) ds

propPruningGeneratedScriptIsNoop :: DynLogicModel s => DynLogic s -> Property
propPruningGeneratedScriptIsNoop d =
  forAll (sized $ \n -> choose (1, max 1 n) >>= generateDLTest d) $ \test ->
    let script = case test of
          BadPrecondition s _ _ -> s
          Looping s -> s
          Stuck s _ -> s
          DLScript s -> s
     in script == pruneDLTest d script

getScript :: DynLogicTest s -> TestSequence s
getScript (BadPrecondition s _ _) = s
getScript (Looping s) = s
getScript (Stuck s _) = s
getScript (DLScript s) = s

makeTestFromPruned :: DynLogicModel s => DynLogic s -> TestSequence s -> DynLogicTest s
makeTestFromPruned dl = make dl initialAnnotatedState
  where
    make d s TestSeqStop
      | b : _ <- badActions d s = BadPrecondition TestSeqStop b s
      | stuck d s = Stuck TestSeqStop s
      | otherwise = DLScript TestSeqStop
    make d s (TestSeqWitness a ss) =
      onDLTestSeq (TestSeqWitness a) $
        make
          (stepDLWitness d a)
          s
          ss
    make d s (TestSeqStep step ss) =
      onDLTestSeq (TestSeqStep step) $
        make
          (stepDLStep d s step)
          (nextStateStep step s)
          ss

-- | If failed, return the prefix up to the failure. Also prunes the test in case the model has
--   changed.
unfailDLTest :: DynLogicModel s => DynLogic s -> DynLogicTest s -> DynLogicTest s
unfailDLTest d test = makeTestFromPruned d $ pruneDLTest d steps
  where
    steps = case test of
      BadPrecondition as _ _ -> as
      Stuck as _ -> as
      DLScript as -> as
      Looping as -> as

stuck :: DynLogicModel s => DynLogic s -> Annotated s -> Bool
stuck EmptySpec _ = True
stuck Stop _ = False
stuck (After _ _) _ = False
stuck (Error _ _) _ = False
stuck (AfterAny _) s =
  not $
    canGenerate
      0.01
      (computeArbitraryAction s)
      ( \case
          Some act ->
            computePrecondition s act
              && not (restrictedPolar act)
      )
stuck (Alt Angelic d d') s = stuck d s && stuck d' s
stuck (Alt Demonic d d') s = stuck d s || stuck d' s
stuck (Stopping d) s = stuck d s
stuck (Weight w d) s = w < never || stuck d s
stuck (ForAll _ _) _ = False
stuck (Monitor _ d) s = stuck d s

validDLTest :: StateModel s => DynLogic s -> DynLogicTest s -> Property -> Property
validDLTest _ Stuck{} _ = False ==> False
validDLTest _ test@DLScript{} p = counterexample (show test) p
validDLTest _ test _ = counterexample (show test) False

scriptFromDL :: DynLogicTest s -> Actions s
scriptFromDL (DLScript s) = Actions $ sequenceSteps s
scriptFromDL _ = Actions []

sequenceSteps :: TestSequence s -> [Step s]
sequenceSteps (TestSeq ss) =
  case discardWitnesses ss of
    ContStop -> []
    ContStep s ss' -> s : sequenceSteps ss'

badActionsGiven :: StateModel s => DynLogic s -> Annotated s -> Witnesses a -> [Witnesses (FailingAction s)]
badActionsGiven Stop _ _ = []
badActionsGiven EmptySpec _ _ = []
badActionsGiven AfterAny{} _ _ = []
badActionsGiven (ForAll _ k) s (Witness a step) =
  case cast a of
    Just a' -> Witness a' <$> badActionsGiven (k a') s step
    _ -> []
badActionsGiven (Alt _ d d') s w = badActionsGiven d s w ++ badActionsGiven d' s w
badActionsGiven (Stopping d) s w = badActionsGiven d s w
badActionsGiven (Weight k d) s w = if k < never then [] else badActionsGiven d s w
badActionsGiven (Monitor _ d) s w = badActionsGiven d s w
badActionsGiven d s (Do _) = Do <$> badActions d s
badActionsGiven Error{} _ _ = []
badActionsGiven After{} _ _ = []

badActions :: StateModel s => DynLogic s -> Annotated s -> [FailingAction s]
badActions EmptySpec _ = []
badActions Stop _ = []
badActions (After a _) s
  | computePrecondition s a = []
  | otherwise = [ActionFail a]
badActions (Error m _) _s = [ErrorFail m]
badActions (AfterAny _) _ = []
badActions (Alt _ d d') s = badActions d s ++ badActions d' s
badActions (Stopping d) s = badActions d s
badActions (Weight w d) s = if w < never then [] else badActions d s
badActions (ForAll _ _) _ = []
badActions (Monitor _ d) s = badActions d s

applyMonitoring :: DynLogicModel s => DynLogic s -> DynLogicTest s -> Property -> Property
applyMonitoring d (DLScript s) p =
  case findMonitoring d initialAnnotatedState s of
    Just f -> f p
    Nothing -> p
applyMonitoring _ Stuck{} p = p
applyMonitoring _ Looping{} p = p
applyMonitoring _ BadPrecondition{} p = p

findMonitoring :: DynLogicModel s => DynLogic s -> Annotated s -> TestSequence s -> Maybe (Property -> Property)
findMonitoring Stop _s TestSeqStop = Just id
findMonitoring (After a k) s (TestSeqStep (var := a') as)
  -- TODO: do nicely with eqT instead (avoids `unsafeCoerceVar`)
  | Some a == Some a' = findMonitoring (k (unsafeCoerceVar var) s') s' as
  where
    s' = computeNextState s a' (unsafeCoerceVar var)
findMonitoring (AfterAny k) s as@(TestSeqStep (_var := a) _)
  | not (restrictedPolar a) = findMonitoring (After a $ const k) s as
findMonitoring (Alt _b d d') s as =
  -- Give priority to monitoring matches to the left. Combining both
  -- results in repeated monitoring from always, which is unexpected.
  findMonitoring d s as <|> findMonitoring d' s as
findMonitoring (Stopping d) s as = findMonitoring d s as
findMonitoring (Weight _ d) s as = findMonitoring d s as
findMonitoring (ForAll (_q :: Quantification a) k) s (TestSeq (Witness (a :: a') as)) =
  case eqT @a @a' of
    Just Refl -> findMonitoring (k a) s (TestSeq as)
    Nothing -> Nothing
findMonitoring (Monitor m d) s as =
  (m .) <$> findMonitoring d s as
findMonitoring _ _ _ = Nothing

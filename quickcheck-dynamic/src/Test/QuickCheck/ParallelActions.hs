{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- | Experimental support for running parallel actions
module Test.QuickCheck.ParallelActions (
  RunModelPar (..),
  Forking (..),
  ParallelActions,
  runParActions,
) where

import Control.Arrow (first, second)
import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Data
import Data.Set qualified as Set
import Data.Tree
import GHC.Generics
import Test.QuickCheck hiding (Some)
import Test.QuickCheck.Monadic
import Test.QuickCheck.StateModel
import Test.QuickCheck.StateModel.Variables

-- | The callbacks necessary to run actions in parallel
class RunModel state m => RunModelPar state m where
  -- | Note that this version of `perform` doesn't get the current `state`.
  -- This is because the system is not in a definite model state during
  -- parallel execution.
  performPar :: Typeable a => Action state a -> LookUp -> m (PerformResult state m a)
  performPar = perform (error "Trying to evaluate state in default implementation of performPar")

  -- | Like `monitoring` but without the `state`
  monitoringPar :: Action state a -> LookUp -> Either (Error state m) a -> Property -> Property
  monitoringPar _ _ _ = id

data ParallelActions state
  = ParallelActions
  { linearActions :: Actions state
  , threads :: [[Int]]
  }
  deriving (Eq, Generic)

commonActions :: ParallelActions state -> [Step state]
commonActions ParallelActions{linearActions = Actions steps, ..} =
  [step | step@(v := _) <- steps, notElem (unsafeVarIndex v) $ concat threads]

threadActions :: ParallelActions state -> [[Step state]]
threadActions ParallelActions{linearActions = Actions steps, ..} =
  [ [ v := s{polarity = PosPolarity}
    | v := s <- steps
    , elem (unsafeVarIndex v) thread
    ]
  | thread <- threads
  ]

firstParAction :: ParallelActions state -> Maybe Int
firstParAction ParallelActions{..}
  | null idxs = Nothing
  | otherwise = Just $ minimum idxs
  where
    idxs = concat threads

instance StateModel state => Show (ParallelActions state) where
  show pas =
    unlines $
      [ "-- Common Prefix:"
      , showWithUsed (foldMap allVariables threads) common
      ]
        ++ concat
          [ [ "-- Thread " ++ [n]
            , show thread
            ]
          | (n, thread) <- zip ['A' .. 'Z'] threads
          ]
    where
      common = Actions $ commonActions pas
      threads = Actions <$> threadActions pas

instance StateModel state => Arbitrary (ParallelActions state) where
  arbitrary = genParActions

  shrink pas@(ParallelActions actions trs) =
    [ParallelActions actions $ map (filter (/= i)) trs | Just i <- [firstParAction pas]]
      ++ filter
        checkParallelActions
        [ ParallelActions actions $ filter (not . null) $ map (filter (`Set.member` vars)) trs
        | actions <- shrink actions
        , let vars = unsafeIndexSet $ allVariables actions
        ]

checkParallelActions :: StateModel state => ParallelActions state -> Bool
checkParallelActions pas = all (checkWellTypedness commonCtx) (threadActions pas)
  where
    commonCtx = allVariables common
    common = Actions $ commonActions pas

checkWellTypedness :: StateModel state => VarContext -> [Step state] -> Bool
checkWellTypedness _ [] = True
checkWellTypedness ctx ((v := a) : ss) = a `wellTypedIn` ctx && checkWellTypedness (extendContext ctx v) ss

genParActions :: forall state. StateModel state => Gen (ParallelActions state)
genParActions = do
  -- The ~ works around a bug in ghc (https://gitlab.haskell.org/ghc/ghc/-/issues/22004) (which is not in all ghc versions ?!)
  as@(~(Actions steps)) <- arbitrary
  let n = length steps
  split <- choose (max 0 (n - 20), n - 1)
  let (common, post) = splitAt split steps
      commonCtx = allVariables common
  tc <- choose (2, 5)
  threads <- go post $ replicate tc (commonCtx, [])
  return $ ParallelActions as $ filter (not . null) threads
  where
    go :: [Step state] -> [(VarContext, [Int])] -> Gen [[Int]]
    go [] trs = return $ map (reverse . snd) trs
    go ((v := a) : ss) trs = do
      let candidates =
            [ (ctx, tr, trs)
            | ((ctx, tr), trs) <- holes trs
            , a `wellTypedIn` ctx
            ]
      if null candidates
        -- This means we made a mistake earlier and split two actions whose
        -- result variables were used together later. At this point we just
        -- give up and don't extend the traces.
        then go [] trs
        else do
          (ctx, tr, trs) <- elements candidates
          go ss $ (extendContext ctx v, unsafeVarIndex v : tr) : trs

data TraceStep state m where
  TraceStep
    :: (Typeable a, Show a)
    => Either (Error state m) a
    -> Var a
    -> ActionWithPolarity state a
    -> TraceStep state m

type Trace state m = [TraceStep state m]
type TraceTree state m = Tree (TraceStep state m)

runTracing
  :: ( RunModelPar state m
     , e ~ Error state m
     , forall a. IsPerformResult e a
     )
  => Env -> [Step state] -> m (Trace state m, Env)
runTracing env [] = return ([], env)
runTracing env ((v := ap) : as) = do
  r <- performResultToEither <$> performPar (polarAction ap) (lookUpVar env)
  let step = TraceStep r v ap
      env'
        | Right val <- r = (v :== val) : env
        | otherwise = env
  (first (step :)) <$> runTracing env' as

class Monad m => Forking m where
  forkThread :: m a -> m (m a)

instance Forking IO where
  forkThread io = do
    t <- newEmptyMVar
    forkIO $ io >>= putMVar t
    return $ takeMVar t

instance Forking m => Forking (ReaderT r m) where
  forkThread m = do
    reg <- ask
    lift $ fmap lift (forkThread $ runReaderT m reg)

-- | Run parallel actions consisting of a common prefix and a number of
-- parallel threads. After execution check that the preconditions were
-- respected in all possible parallel executions and check that we find at
-- least one parallel execution which is linearizible.
runParActions
  :: ( StateModel state
     , RunModelPar state m
     , e ~ Error state m
     , forall a. IsPerformResult e a
     , Forking m
     )
  => ParallelActions state -> PropertyM m ()
runParActions pas = do
  (trC, env) <- run $ runTracing mempty $ commonActions pas
  joins <- mapM (run . forkThread . runTracing env) (threadActions pas)
  trs <- mapM (fmap fst . run) joins
  let used = varsUsedInActions $ linearActions pas
  monitor $ counterexample "-- Main thread:"
  monitorTrace used mempty trC
  forM (zip ['A' .. 'Z'] trs) $ \(n, tr) -> do
    monitor $ counterexample $ "\n-- Thread " ++ [n, ':']
    monitorTrace used env tr
  let ilvs = prepend trC $ interleavings trs
  monitor $ tabulate "Trace tree size" (map (bucket . length) ilvs)
  assert $ null ilvs || any (checkTrace initialAnnotatedState mempty) ilvs

monitorTrace
  :: forall state m
   . (StateModel state, RunModelPar state m)
  => VarContext -> Env -> Trace state m -> PropertyM m ()
monitorTrace _used _env [] = pure ()
monitorTrace used env (TraceStep r v act : tr) = do
  let showR (Right x)
        | v `wellTypedIn` used = show v ++ "@" ++ showsPrec 10 x ""
        | otherwise = show x
      showR (Left err) = "fail " ++ showsPrec 10 err ""
  monitor $ counterexample (showR r ++ " <- " ++ show (polarAction act))
  monitor $ monitoringPar @state @m (polarAction act) (lookUpVar env) r
  monitorTrace used env' tr
  where
    env'
      | Right val <- r = (v :== val) : env
      | otherwise = env

checkTrace
  :: forall state m
   . (StateModel state, RunModelPar state m)
  => Annotated state -> Env -> TraceTree state m -> Bool
checkTrace s env (Node (TraceStep r v (ActionWithPolarity a _)) trs) =
  -- NOTE: we need to re-compute the polarity of `a` here because it may be that the failure can be explained,
  -- but only by the action failing when it was previous successful
  let act = actionWithPolarity s a
      s' = computeNextState s act v
      env'
        | Right val <- r = (v :== val) : env
        | otherwise = env
      checkPost
        | Right val <- r
        , polarity act == PosPolarity =
            fst . runWriter . runPost $
              postcondition @state @m
                (underlyingState s, underlyingState s')
                (polarAction act)
                (lookUpVar env')
                val
        | Left{} <- r, polarity act == PosPolarity = False
        | otherwise =
            fst . runWriter . runPost $
              postconditionOnFailure @state @m
                (underlyingState s, underlyingState s')
                (polarAction act)
                (lookUpVar env')
                r
   in (computePrecondition s act || discard) && checkPost && (null trs || any (checkTrace s' env') trs)

prepend :: [a] -> [Tree a] -> [Tree a]
prepend [] ts = ts
prepend (p : ps) ts = [Node p $ prepend ps ts]

interleavings :: [[a]] -> [Tree a]
interleavings aas = do
  (a : as, os) <- holes aas
  pure $ Node a (interleavings (as : os))

holes :: [a] -> [(a, [a])]
holes [] = []
holes (a : as) = (a, as) : map (second (a :)) (holes as)

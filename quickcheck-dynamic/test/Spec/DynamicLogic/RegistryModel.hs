{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.DynamicLogic.RegistryModel where

import Control.Concurrent.Class.MonadSTM
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadTest
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTimer
import Control.Monad.IOSim

import GHC.Generics
import Unsafe.Coerce

import Control.Monad.Reader
import Data.Either
import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Typeable
import Test.QuickCheck as QC hiding (Some)
import Test.QuickCheck.Gen.Unsafe (Capture (..), capture)
import Test.QuickCheck.Monadic hiding (assert)
import Test.Tasty hiding (after)

import Test.Tasty.QuickCheck (testProperty)

import Spec.DynamicLogic.Registry
import Test.QuickCheck.DynamicLogic
import Test.QuickCheck.Extras
import Test.QuickCheck.ParallelActions
import Test.QuickCheck.StateModel

type GoodMonad m =
  ( Typeable (ThreadId m)
  , Show (ThreadId m)
  , MonadSTM m
  , MonadFork m
  , MonadDelay m
  , MonadCatch m
  , MonadThrow (STM m)
  )

data RegState m = RegState
  { regs :: Map String (Var (ThreadId m))
  , dead :: [Var (ThreadId m)]
  }
  deriving (Show, Generic)

deriving instance Show (Action (RegState m) a)
deriving instance Eq (Action (RegState m) a)

instance GoodMonad m => HasVariables (Action (RegState m) a) where
  getAllVariables (Register _ v) = getAllVariables v
  getAllVariables (KillThread v) = getAllVariables v
  getAllVariables _ = mempty

instance GoodMonad m => StateModel (RegState m) where
  data Action (RegState m) a where
    Spawn :: Action (RegState m) (ThreadId m)
    WhereIs :: String -> Action (RegState m) (Maybe (ThreadId m))
    Register :: String -> Var (ThreadId m) -> Action (RegState m) ()
    Unregister :: String -> Action (RegState m) ()
    KillThread :: Var (ThreadId m) -> Action (RegState m) ()

  precondition s (Register name tid) =
    name `Map.notMember` regs s
      && tid `notElem` Map.elems (regs s)
      && tid `notElem` dead s
  precondition s (Unregister name) =
    name `Map.member` regs s
  precondition _ _ = True

  validFailingAction _ _ = True

  arbitraryAction ctx s =
    let threadIdCtx = ctxAtType @(ThreadId m) ctx
     in frequency $
          [
            ( max 1 $ 10 - length threadIdCtx
            , return $ Some Spawn
            )
          ,
            ( 2 * Map.size (regs s)
            , Some <$> (Unregister <$> probablyRegistered s)
            )
          ,
            ( 10
            , Some <$> (WhereIs <$> probablyRegistered s)
            )
          ]
            ++ [ ( max 1 $ 3 - length (dead s)
                 , Some <$> (KillThread <$> arbitraryVar ctx)
                 )
               | not . null $ threadIdCtx
               ]
            ++ [ ( max 1 $ 10 - Map.size (regs s)
                 , Some <$> (Register <$> probablyUnregistered s <*> arbitraryVar ctx)
                 )
               | not . null $ threadIdCtx
               ]

  shrinkAction ctx _ (Register name tid) =
    [Some (Unregister name)]
      ++ [Some (Register name' tid) | name' <- shrinkName name]
      ++ [Some (Register name tid') | tid' <- shrinkVar ctx tid]
  shrinkAction _ _ (Unregister name) =
    Some (WhereIs name) : [Some (Unregister name') | name' <- shrinkName name]
  shrinkAction _ _ (WhereIs name) =
    [Some (WhereIs name') | name' <- shrinkName name]
  shrinkAction _ _ Spawn = []
  shrinkAction ctx _ (KillThread tid) =
    [Some (KillThread tid') | tid' <- shrinkVar ctx tid]

  initialState = RegState mempty []

  nextState s Spawn _ = s
  nextState s (Register name tid) _step = s{regs = Map.insert name tid (regs s)}
  nextState s (Unregister name) _step =
    s{regs = Map.delete name (regs s)}
  nextState s (KillThread tid) _ =
    s
      { dead = tid : dead s
      , regs = Map.filter (/= tid) (regs s)
      }
  nextState s WhereIs{} _ = s

type RegM m = ReaderT (Registry m) m

instance GoodMonad m => RunModel (RegState m) (RegM m) where
  type Error (RegState m) (RegM m) = String

  perform _ Spawn _ = do
    reg <- ask
    tid <- lift $ spawn reg (threadDelay 10000000)
    pure $ Right tid
  perform _ (Register name tid) env = do
    reg <- ask
    lift $ fmap (either (Left . displayException . toException @SomeException) Right) $ try $ register reg name (env tid)
  perform _ (Unregister name) _ = do
    reg <- ask
    lift $ fmap (either (Left . displayException . toException @SomeException) Right) $ try $ unregister reg name
  perform _ (WhereIs name) _ = do
    reg <- ask
    res <- lift $ whereis reg name
    pure $ Right res
  perform _ (KillThread tid) env = do
    lift $ killThread (env tid)
    lift $ threadDelay 100
    pure $ Right ()

  postcondition (s, _) (WhereIs name) env mtid = do
    pure $ (env <$> Map.lookup name (regs s)) == mtid
  postcondition _ _ _ _ = pure True

  postconditionOnFailure (s, _) act@Register{} _ res = do
    monitorPost $
      tabulate
        "Reason for -Register"
        [why s act]
    pure $ isLeft res
  postconditionOnFailure _s _ _ _ = pure True

  monitoring (_s, s') act@(showDictAction -> ShowDict) _ res =
    counterexample (show res ++ " <- " ++ show act ++ "\n  -- State: " ++ show s')
      . tabulate "Registry size" [show $ Map.size (regs s')]

-- NOTE: We rely on the default implementation of `performPar` here because
-- `perform` doesn't actually look at the state.
instance GoodMonad m => RunModelPar (RegState m) (RegM m)

data ShowDict a where
  ShowDict :: Show a => ShowDict a

showDictAction :: forall m a. GoodMonad m => Action (RegState m) a -> ShowDict a
showDictAction Spawn{} = ShowDict
showDictAction WhereIs{} = ShowDict
showDictAction Register{} = ShowDict
showDictAction Unregister{} = ShowDict
showDictAction KillThread{} = ShowDict

instance GoodMonad m => DynLogicModel (RegState m) where
  restricted _ = False

instance Forking (IOSim s) where
  forkThread io = do
    t <- atomically newEmptyTMVar
    forkIO $ io >>= atomically . putTMVar t
    return $ atomically $ takeTMVar t

why :: (RegState m) -> Action (RegState m) a -> String
why s (Register name tid) =
  unwords $
    ["name already registered" | name `Map.member` regs s]
      ++ ["tid already registered" | tid `elem` Map.elems (regs s)]
      ++ ["dead thread" | tid `elem` dead s]
why _ _ = "(impossible)"

arbitraryName :: Gen String
arbitraryName = elements allNames

probablyRegistered :: (RegState m) -> Gen String
probablyRegistered s = oneof $ map pure (Map.keys $ regs s) ++ [arbitraryName]

probablyUnregistered :: (RegState m) -> Gen String
probablyUnregistered s = elements $ allNames ++ (allNames \\ Map.keys (regs s))

shrinkName :: String -> [String]
shrinkName name = [n | n <- allNames, n < name]

allNames :: [String]
allNames = ["a", "b", "c", "d", "e"]

prop_Registry :: Actions (RegState IO) -> Property
prop_Registry s =
  monadicIO $ do
    monitor $ counterexample "\nExecution\n"
    reg <- lift setupRegistry
    runPropertyReaderT (runActions s) reg
    return True

prop_parRegistry :: ParallelActions (RegState IO) -> Property
prop_parRegistry as =
  monadicIO $ do
    reg <- lift setupRegistry
    runPropertyReaderT (runParActions as) reg

newtype IOSimActions = IOSimActions (forall s. ParallelActions (RegState (IOSim s)))

instance Arbitrary IOSimActions where
  arbitrary = do
    Capture eval <- capture
    pure $ IOSimActions (eval arbitrary)

  shrink (IOSimActions acts) = [IOSimActions (unsafeCoerce acts) | acts <- shrink acts]

instance Show IOSimActions where
  show (IOSimActions acts) = show acts

prop_parRegistryIOSim :: IOSimActions -> Property
prop_parRegistryIOSim (IOSimActions as) = monadicIOSim_ prop
  where
    prop :: forall s. PropertyM (IOSim s) ()
    prop = do
      reg <- lift setupRegistry
      runPropertyReaderT (runParActions $ as @s) reg
      pure ()

prop_parRegistryIOSimPor :: IOSimActions -> Property
prop_parRegistryIOSimPor (IOSimActions as) =
  monadicIOSimPOR_ prop
  where
    prop :: forall s. PropertyM (IOSim s) ()
    prop = do
      reg <- lift setupRegistry
      lift exploreRaces
      runPropertyReaderT (runParActions $ as @s) reg
      pure ()

-- NOTE: This is a hack to get around issues with old ghc versions
data Exists where
  Ex :: (forall s. IOSim s Property) -> Exists

monadicIOSimPOR_ :: Testable a => (forall s. PropertyM (IOSim s) a) -> Property
monadicIOSimPOR_ prop = forAllBlind prop' $ \(Ex p) -> exploreSimTrace id p $ \_ tr ->
  either discard id $ traceResult False tr
  where
    prop' :: Gen Exists
    prop' = do
      Capture eval <- capture
      pure $ Ex $ eval $ monadic' prop

propDL :: DL (RegState IO) () -> Property
propDL d = forAllDL d prop_Registry

-- DL helpers

unregisterNameAndTid :: String -> Var (ThreadId m) -> DL (RegState m) ()
unregisterNameAndTid name tid = do
  s <- getModelStateDL
  sequence_
    [ action $ Unregister name'
    | (name', tid') <- Map.toList $ regs s
    , name' == name || tid' == tid
    ]

unregisterTid :: Var (ThreadId m) -> DL (RegState m) ()
unregisterTid tid = do
  s <- getModelStateDL
  sequence_
    [ action $ Unregister name
    | (name, tid') <- Map.toList $ regs s
    , tid' == tid
    ]

getAlive :: forall m. GoodMonad m => DL (RegState m) [Var (ThreadId m)]
getAlive = do
  s <- getModelStateDL
  ctx <- getVarContextDL
  pure $ ctxAtType @(ThreadId m) ctx \\ dead s

pickThread :: forall m. GoodMonad m => DL (RegState m) (Var (ThreadId m))
pickThread = do
  tids <- ctxAtType @(ThreadId m) <$> getVarContextDL
  forAllQ $ elementsQ tids

pickAlive :: GoodMonad m => DL (RegState m) (Var (ThreadId m))
pickAlive = do
  alive <- getAlive
  forAllQ $ elementsQ alive

pickFreshName :: DL (RegState m) String
pickFreshName = do
  used <- Map.keys . regs <$> getModelStateDL
  forAllQ $ elementsQ (allNames \\ used)

-- test that the registry never contains more than k processes

regLimit :: Int -> DL (RegState m) ()
regLimit k = do
  anyActions_
  assertModel "Too many processes" $ \s -> Map.size (regs s) <= k

-- test that we can register a pid that is not dead, if we unregister the name first.

canRegisterAlive :: GoodMonad m => String -> DL (RegState m) ()
canRegisterAlive name = do
  tid <- pickAlive
  unregisterNameAndTid name tid
  action $ Register name tid
  pure ()

canRegister :: GoodMonad m => DL (RegState m) ()
canRegister = do
  anyActions_
  name <- pickFreshName
  canRegisterAlive name

canRegisterNoUnregister :: GoodMonad m => DL (RegState m) ()
canRegisterNoUnregister = do
  anyActions_
  name <- pickFreshName
  tid <- pickAlive
  action $ Register name tid
  pure ()

tests :: TestTree
tests =
  testGroup
    "registry model example"
    [ testProperty "prop_Registry" $ prop_Registry
    , testProperty "moreActions 10 $ prop_Registry" $ moreActions 10 $ prop_Registry
    , testProperty "canRegister" $ propDL canRegister
    , testProperty "canRegisterNoUnregister" $ expectFailure $ propDL canRegisterNoUnregister
    , testProperty "prop_parRegistryIOSimPor" $ expectFailure $ withMaxSuccess 10000 $ discardAfter 1000 $ prop_parRegistryIOSimPor
    ]

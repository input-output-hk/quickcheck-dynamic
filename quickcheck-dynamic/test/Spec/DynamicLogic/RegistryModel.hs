module Spec.DynamicLogic.RegistryModel where

import Control.Concurrent
import Control.Exception

import GHC.Generics

import Control.Monad.Reader
import Data.Either
import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Test.QuickCheck
import Test.QuickCheck.Monadic hiding (assert)
import Test.QuickCheck.Monadic qualified as QC
import Test.Tasty hiding (after)

import Test.Tasty.QuickCheck (testProperty)

import Spec.DynamicLogic.Registry
import Test.QuickCheck.DynamicLogic
import Test.QuickCheck.Extras
import Test.QuickCheck.StateModel

data RegState = RegState
  { regs :: Map String (Var ThreadId)
  , dead :: [Var ThreadId]
  }
  deriving (Show, Generic)

deriving instance Show (Action RegState a)
deriving instance Eq (Action RegState a)

instance HasVariables (Action RegState a) where
  getAllVariables (Register _ v) = getAllVariables v
  getAllVariables (KillThread v) = getAllVariables v
  getAllVariables _ = mempty

instance StateModel RegState where
  data Action RegState a where
    Spawn :: Action RegState ThreadId
    WhereIs :: String -> Action RegState (Maybe ThreadId)
    Register :: String -> Var ThreadId -> Action RegState ()
    Unregister :: String -> Action RegState ()
    KillThread :: Var ThreadId -> Action RegState ()

  type Error RegState = SomeException

  precondition s (Register name tid) =
    name `Map.notMember` regs s
      && tid `notElem` Map.elems (regs s)
      && tid `notElem` dead s
  precondition s (Unregister name) =
    name `Map.member` regs s
  precondition _ _ = True

  validFailingAction _ _ = True

  arbitraryAction ctx s =
    frequency $
      [
        ( max 1 $ 10 - length (ctxAtType @ThreadId ctx)
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
           | not . null $ ctxAtType @ThreadId ctx
           ]
        ++ [ ( max 1 $ 10 - Map.size (regs s)
             , Some <$> (Register <$> probablyUnregistered s <*> arbitraryVar ctx)
             )
           | not . null $ ctxAtType @ThreadId ctx
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

type RegM = ReaderT Registry IO

instance RunModel RegState RegM where
  perform _ Spawn _ = do
    tid <- lift $ forkIO (threadDelay 10000000)
    pure $ Right tid
  perform _ (Register name tid) env = do
    reg <- ask
    lift $ try $ register reg name (env tid)
  perform _ (Unregister name) _ = do
    reg <- ask
    lift $ try $ unregister reg name
  perform _ (WhereIs name) _ = do
    reg <- ask
    res <- lift $ whereis reg name
    pure $ Right res
  perform _ (KillThread tid) env = do
    lift $ killThread (env tid)
    lift $ threadDelay 100
    pure $ Right ()

  perform' Spawn _ = do
    tid <- lift $ forkIO (threadDelay 10000000)
    pure $ Right tid
  perform' (Register name tid) env = do
    reg <- ask
    lift $ try $ register reg name (env tid)
  perform' (Unregister name) _ = do
    reg <- ask
    lift $ try $ unregister reg name
  perform' (WhereIs name) _ = do
    reg <- ask
    res <- lift $ whereis reg name
    pure $ Right res
  perform' (KillThread tid) env = do
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

data ShowDict a where
  ShowDict :: Show (Realized RegM a) => ShowDict a

showDictAction :: forall a. Action RegState a -> ShowDict a
showDictAction Spawn{} = ShowDict
showDictAction WhereIs{} = ShowDict
showDictAction Register{} = ShowDict
showDictAction Unregister{} = ShowDict
showDictAction KillThread{} = ShowDict

instance DynLogicModel RegState where
  restricted _ = False

why :: RegState -> Action RegState a -> String
why s (Register name tid) =
  unwords $
    ["name already registered" | name `Map.member` regs s]
      ++ ["tid already registered" | tid `elem` Map.elems (regs s)]
      ++ ["dead thread" | tid `elem` dead s]
why _ _ = "(impossible)"

arbitraryName :: Gen String
arbitraryName = elements allNames

probablyRegistered :: RegState -> Gen String
probablyRegistered s = oneof $ map pure (Map.keys $ regs s) ++ [arbitraryName]

probablyUnregistered :: RegState -> Gen String
probablyUnregistered s = elements $ allNames ++ (allNames \\ Map.keys (regs s))

shrinkName :: String -> [String]
shrinkName name = [n | n <- allNames, n < name]

allNames :: [String]
allNames = ["a", "b", "c", "d", "e"]

prop_Registry :: Actions RegState -> Property
prop_Registry s =
  monadicIO $ do
    monitor $ counterexample "\nExecution\n"
    reg <- lift setupRegistry
    runPropertyReaderT (runActions s) reg
    QC.assert True

propDL :: DL RegState () -> Property
propDL d = forAllDL d prop_Registry

-- DL helpers

unregisterNameAndTid :: String -> Var ThreadId -> DL RegState ()
unregisterNameAndTid name tid = do
  s <- getModelStateDL
  sequence_
    [ action $ Unregister name'
    | (name', tid') <- Map.toList $ regs s
    , name' == name || tid' == tid
    ]

unregisterTid :: Var ThreadId -> DL RegState ()
unregisterTid tid = do
  s <- getModelStateDL
  sequence_
    [ action $ Unregister name
    | (name, tid') <- Map.toList $ regs s
    , tid' == tid
    ]

getAlive :: DL RegState [Var ThreadId]
getAlive = do
  s <- getModelStateDL
  ctx <- getVarContextDL
  pure $ ctxAtType @ThreadId ctx \\ dead s

pickThread :: DL RegState (Var ThreadId)
pickThread = do
  tids <- ctxAtType @ThreadId <$> getVarContextDL
  forAllQ $ elementsQ tids

pickAlive :: DL RegState (Var ThreadId)
pickAlive = do
  alive <- getAlive
  forAllQ $ elementsQ alive

pickFreshName :: DL RegState String
pickFreshName = do
  used <- Map.keys . regs <$> getModelStateDL
  forAllQ $ elementsQ (allNames \\ used)

-- test that the registry never contains more than k processes

regLimit :: Int -> DL RegState ()
regLimit k = do
  anyActions_
  assertModel "Too many processes" $ \s -> Map.size (regs s) <= k

-- test that we can register a pid that is not dead, if we unregister the name first.

canRegisterAlive :: String -> DL RegState ()
canRegisterAlive name = do
  tid <- pickAlive
  unregisterNameAndTid name tid
  action $ Register name tid
  pure ()

canRegister :: DL RegState ()
canRegister = do
  anyActions_
  name <- pickFreshName
  canRegisterAlive name

canRegisterNoUnregister :: DL RegState ()
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
    , testProperty "canRegister" $ propDL canRegister
    , testProperty "canRegisterNoUnregister" $ expectFailure $ propDL canRegisterNoUnregister
    ]

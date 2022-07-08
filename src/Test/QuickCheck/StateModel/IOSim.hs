{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.QuickCheck.StateModel.IOSim where

-- TODO: It's possible that we can get away with only using `io-classes` as a dependency here. That would greatly help.

import Control.Lens
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadSTM
import Control.Monad.IOSim
import Control.Monad.State

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Typeable

import Test.QuickCheck.Monadic

-- Utility functions

invert :: Eq v => Map k v -> v -> Maybe k
invert map v = lookup v [(v, k) | (k, v) <- Map.toList map]

data FEnvEntry f where
  (:==) :: (Typeable a, Eq (f a)) => Int -> f a -> FEnvEntry f

type FEnv f = [FEnvEntry f]

lookupFEnv :: Typeable a => Int -> FEnv f -> Maybe (f a)
lookupFEnv _ [] = Nothing
lookupFEnv i ((j :== fa) : env)
  | i == j, Just fa' <- gcast fa = Just fa'
  | otherwise = lookupFEnv i env

invertFEnv :: Typeable a => FEnv f -> f a -> Maybe Int
invertFEnv [] _ = Nothing
invertFEnv ((i :== fa) : env) fa'
  | Just fa'' <- gcast fa', fa'' == fa = Just i
  | otherwise = invertFEnv env fa'

-- IOSim model stuff

data MThreadId
data MTVar (a :: *)

data ModelEnv s = ModelEnv
  { _threadIdEnv :: Map Int (ThreadId (IOSim s))
  , _tvarEnv :: FEnv (TVar (IOSim s))
  }
makeLenses 'ModelEnv

initialModelEnv :: ModelEnv s
initialModelEnv =
  ModelEnv
    { _threadIdEnv = mempty
    , _tvarEnv = mempty
    }

newtype Model (t :: k) = Model Int deriving (Eq, Ord)

instance Show (Model t) where
  show (Model i) = "Model[" ++ show i ++ "]"

type ModelThreadId = Model MThreadId

newtype Concrete t = Concrete t

class Instantiate t where
  type Instantiated t s :: *
  instantiate :: t -> ModelEnv s -> Instantiated t s
  encapsulate :: Instantiated t s -> ModelEnv s -> (t, ModelEnv s)

-- TODO: can we generalize this to avoid copy-paste and allow us to work with IOSim and IO at the same time?
instance Instantiate (Model MThreadId) where
  type Instantiated (Model MThreadId) s = ThreadId (IOSim s)
  instantiate (Model idx) env = fromJust $ env ^. threadIdEnv . at idx -- TODO error message
  encapsulate t env =
    case invert (env ^. threadIdEnv) t of
      Nothing ->
        let idx
              | null $ env ^. threadIdEnv = 0
              | otherwise = 1 + fst (Map.findMax (env ^. threadIdEnv))
            env' = env & threadIdEnv . at idx .~ Just t
         in (Model idx, env')
      Just k -> (Model k, env)

instance (Typeable a) => Instantiate (Model (MTVar a)) where
  type Instantiated (Model (MTVar a)) s = TVar (IOSim s) a
  instantiate (Model idx) env = fromJust . lookupFEnv idx $ env ^. tvarEnv -- TODO error message
  encapsulate t env =
    case invertFEnv (env ^. tvarEnv) t of
      Nothing ->
        let idx = length (env ^. tvarEnv)
            env' = env & tvarEnv %~ (idx :== t :)
         in (Model idx, env')
      Just k -> (Model k, env)

instance Instantiate (Concrete t) where
  type Instantiated (Concrete t) s = t
  instantiate (Concrete t) _ = t
  encapsulate t env = (Concrete t, env)

-- TODO: this is all a bit awkward and not necessarily
-- the final version of this. For example, the Instantiate instance for Maybe means that
-- there shouldn't really be any difference between `Var (Model (Maybe t))` and `Var (Maybe (Model t))`.
--
-- In general, it might be wiser to abandon this ad-hoc stuff *on top* of `Var` and just do it with
-- `Var` instead. You could have `Instantiated (Var (Concrete t)) s = t` and `Instantiated (Var MThreadId) s =
-- ThreadId (IOSim s)` for example.

instance Instantiate t => Instantiate (Maybe t) where
  type Instantiated (Maybe t) s = Maybe (Instantiated t s)
  instantiate Nothing _ = Nothing
  instantiate (Just t) env = Just (instantiate t env)

  encapsulate Nothing env = (Nothing, env)
  encapsulate (Just t) env = let (t', env') = encapsulate t env in (Just t', env')

data ModelEnvState st s = ModelEnvState st (ModelEnv s)

initialModelEnvState :: st -> ModelEnvState st s
initialModelEnvState st = ModelEnvState st initialModelEnv

-- TODO: make synonym with proper state instance
newtype IOSimModel st s a = IOSimModel {unIOSimModel :: StateT (ModelEnvState st s) (IOSim s) a}
  deriving newtype (Functor, Applicative, Monad)

instance MonadState st (IOSimModel st s) where
  get = IOSimModel $ do ModelEnvState st _ <- get; return st
  put st = IOSimModel $ do modify $ \(ModelEnvState _ env) -> ModelEnvState st env

liftIOSim :: IOSim s a -> IOSimModel st s a
liftIOSim m = IOSimModel $ lift m

runPropertyIOSim :: PropertyM (IOSimModel st s) a -> st -> PropertyM (IOSim s) (a, st)
runPropertyIOSim p st = MkPropertyM $ \k -> do
  m <- unPropertyM (do a <- p; st <- run get; return (a, st)) $ fmap liftIOSim . k
  return $ evalStateT (unIOSimModel m) (initialModelEnvState st)

instantiateM :: Instantiate t => t -> IOSimModel st s (Instantiated t s)
instantiateM t = do
  ModelEnvState _ env <- IOSimModel $ get
  return (instantiate t env)

encapsulateM :: Instantiate t => IOSim s (Instantiated t s) -> IOSimModel st s t
encapsulateM m = do
  ModelEnvState st env <- IOSimModel $ get
  tInst <- IOSimModel $ lift m
  let (t, env') = encapsulate tInst env
  IOSimModel $ put (ModelEnvState st env')
  return t

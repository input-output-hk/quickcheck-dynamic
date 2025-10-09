-- A simple local name service for threads... behaves like the Erlang
-- process registry.
module Spec.DynamicLogic.Registry where

import Control.Concurrent.Class.MonadSTM
import Control.Exception.Base (pattern ErrorCall)
import Control.Monad
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadThrow
import Data.List qualified as List

data Reg m = Reg
  { registered :: [(String, ThreadId m)]
  , aliveThreads :: [ThreadId m]
  }

emptyReg :: Reg m
emptyReg = Reg [] []

modifyRegistered :: ([(String, ThreadId m)] -> [(String, ThreadId m)]) -> Reg m -> Reg m
modifyRegistered f reg = reg{registered = f (registered reg)}

modifyAlive :: ([ThreadId m] -> [ThreadId m]) -> Reg m -> Reg m
modifyAlive f reg = reg{aliveThreads = f (aliveThreads reg)}

newtype Registry m = Registry (TVar m (Reg m))

readReg :: MonadSTM m => Registry m -> STM m (Reg m)
readReg (Registry r) = readTVar r

writeReg :: MonadSTM m => Registry m -> Reg m -> STM m ()
writeReg (Registry r) = writeTVar r

modifyReg :: MonadSTM m => Registry m -> (Reg m -> Reg m) -> STM m ()
modifyReg (Registry r) = modifyTVar r

type MonadRegistry m = (MonadSTM m, MonadFork m, MonadThrow m, MonadThrow (STM m))

isAlive :: MonadRegistry m => Registry m -> ThreadId m -> m Bool
isAlive registry tid =
  elem tid . aliveThreads <$> atomically (readReg registry)

setupRegistry :: forall m. MonadRegistry m => m (Registry m)
setupRegistry = atomically $ Registry <$> newTVar @m emptyReg

spawn :: MonadRegistry m => Registry m -> m () -> m (ThreadId m)
spawn registry run = do
  sync <- atomically newEmptyTMVar
  let body = do
        self <- myThreadId
        atomically $ do
          modifyReg registry $ modifyAlive (self :)
          writeTMVar sync self
        run
      after _ = do
        self <- myThreadId
        atomically $ modifyReg registry $ modifyAlive $ List.delete self
  forkFinally body after
  atomically $ readTMVar sync

whereis :: MonadRegistry m => Registry m -> String -> m (Maybe (ThreadId m))
whereis registry name = do
  reg <- readRegistry registry
  return $ lookup name reg

register :: MonadRegistry m => Registry m -> String -> ThreadId m -> m ()
register registry name tid = do
  ok <- isAlive registry tid
  reg <- readRegistry registry
  if ok && name `notElem` map fst reg && tid `notElem` map snd reg
    then atomically $ do
      reg' <- registered <$> readReg registry
      if name `notElem` map fst reg' && tid `notElem` map snd reg'
        then modifyReg registry $ \reg -> reg{registered = (name, tid) : reg'}
        else throwIO (ErrorCall "badarg")
    else throwIO (ErrorCall "badarg")

unregister :: MonadRegistry m => Registry m -> String -> m ()
unregister registry name = do
  reg <- readRegistry registry
  when (name `elem` map fst reg) $ do
    atomically $ modifyReg registry $ modifyRegistered $ filter ((/= name) . fst)

readRegistry :: MonadRegistry m => Registry m -> m [(String, ThreadId m)]
readRegistry registry = garbageCollect registry *> atomically (registered <$> readReg registry)

garbageCollect :: forall m. MonadRegistry m => Registry m -> m ()
garbageCollect registry = do
  reg <- registered <$> atomically (readReg @m registry)
  garbage <- filterM (fmap not . isAlive registry) (map snd reg)
  atomically $ modifyReg registry $ modifyRegistered $ filter ((`notElem` garbage) . snd)
  return ()

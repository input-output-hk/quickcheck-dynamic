-- A simple local name service for threads... behaves like the Erlang
-- process registry.
module Spec.DynamicLogic.Registry where

import Control.Concurrent.Class.MonadSTM.TVar
import Control.Monad
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import GHC.Conc (ThreadStatus (..))

type Registry m = TVar m [(String, ThreadId m)]

type MonadRegistry m = (MonadSTM m, MonadFork m, MonadThrow m, MonadFail m, MonadFail (STM m))

isAlive :: MonadRegistry m => ThreadId m -> m Bool
isAlive tid = do
  s <- threadStatus tid
  return $ s /= ThreadFinished && s /= ThreadDied

setupRegistry :: forall m. MonadRegistry m => m (Registry m)
setupRegistry = atomically $ newTVar @m []

whereis :: MonadRegistry m => Registry m -> String -> m (Maybe (ThreadId m))
whereis registry name = do
  reg <- readRegistry registry
  return $ lookup name reg

register :: MonadRegistry m => Registry m -> String -> ThreadId m -> m ()
register registry name tid = do
  ok <- isAlive tid
  reg <- readRegistry registry
  if ok && name `notElem` map fst reg && tid `notElem` map snd reg
    then atomically $ do
      reg' <- readTVar registry
      if name `notElem` map fst reg' && tid `notElem` map snd reg'
        then writeTVar registry ((name, tid) : reg')
        else fail "badarg"
    else fail "badarg"

unregister :: MonadRegistry m => Registry m -> String -> m ()
unregister registry name = do
  reg <- readRegistry registry
  when (name `elem` map fst reg) $ do
    atomically $ modifyTVar registry $ filter ((/= name) . fst)

readRegistry :: MonadRegistry m => Registry m -> m [(String, ThreadId m)]
readRegistry registry = garbageCollect registry *> atomically (readTVar registry)

garbageCollect :: forall m. MonadRegistry m => Registry m -> m ()
garbageCollect registry = do
  reg <- atomically $ readTVar @m registry
  garbage <- filterM (fmap not . isAlive) (map snd reg)
  atomically $ modifyTVar registry $ filter ((`notElem` garbage) . snd)
  return ()

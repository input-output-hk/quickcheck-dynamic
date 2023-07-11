-- A simple local name service for threads... behaves like the Erlang
-- process registry.
module Spec.DynamicLogic.Registry where

import Control.Concurrent.STM
import Control.Monad
import GHC.Conc

type Registry = TVar [(String, ThreadId)]

isAlive :: ThreadId -> IO Bool
isAlive tid = do
  s <- threadStatus tid
  return $ s /= ThreadFinished && s /= ThreadDied

setupRegistry :: IO Registry
setupRegistry = atomically $ newTVar []

whereis :: Registry -> String -> IO (Maybe ThreadId)
whereis registry name = do
  reg <- readRegistry registry
  return $ lookup name reg

register :: Registry -> String -> ThreadId -> IO ()
register registry name tid = do
  ok <- isAlive tid
  reg <- readRegistry registry
  if ok && name `notElem` map fst reg && tid `notElem` map snd reg
    then atomically $ do
      reg' <- readTVar registry
      if name `notElem` map fst reg' && tid `notElem` map snd reg'
        then writeTVar registry ((name, tid) : reg')
        else error "badarg"
    else error "badarg"

unregister :: Registry -> String -> IO ()
unregister registry name = do
  reg <- readRegistry registry
  when (name `elem` map fst reg) $ do
    atomically $ modifyTVar registry $ filter ((/= name) . fst)

readRegistry :: Registry -> IO [(String, ThreadId)]
readRegistry registry = garbageCollect registry *> atomically (readTVar registry)

garbageCollect :: Registry -> IO ()
garbageCollect registry = do
  reg <- atomically $ readTVar registry
  garbage <- filterM (fmap not . isAlive) (map snd reg)
  atomically $ modifyTVar registry $ filter ((`notElem` garbage) . snd)
  return ()

module Test.QuickCheck.Extras where

import Control.Monad.Reader
import Control.Monad.State
import Test.QuickCheck.Monadic

runPropertyStateT :: Monad m => PropertyM (StateT s m) a -> s -> PropertyM m (a, s)
runPropertyStateT p s0 = MkPropertyM $ \k -> do
  m <- unPropertyM (do a <- p; s <- run get; return (a, s)) $ fmap lift . k
  return $ evalStateT m s0

runPropertyReaderT :: Monad m => PropertyM (ReaderT e m) a -> e -> PropertyM m a
runPropertyReaderT p e = MkPropertyM $ \k -> do
  m <- unPropertyM p $ fmap lift . k
  return $ runReaderT m e

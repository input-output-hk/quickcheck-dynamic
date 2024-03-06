{-# LANGUAGE NamedFieldPuns #-}

-- | A Reasonably efficient version of a ring buffer.
-- Directly stolen from [q-s-m](https://github.com/stevana/quickcheck-state-machine/blob/master/test/CircularBuffer.hs#L86)
module Spec.CircularBuffer.Buffer where

import Data.Function (on)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Vector.Unboxed.Mutable (
  IOVector,
 )
import qualified Data.Vector.Unboxed.Mutable as V

-- | An efficient mutable circular buffer.
data Buffer = Buffer
  { top :: IORef Int
  -- ^ Index to the top: where to 'Put' the next element
  , bot :: IORef Int
  -- ^ Index to the bottom: where to 'Get' the next element
  , arr :: IOVector Int
  -- ^ Array of elements of fixed capacity
  }

-- | Different buffers are assumed to have disjoint memories,
-- so we can use 'V.overlaps' to check equality.
instance Eq Buffer where
  (==) =
    ((==) `on` top)
      `also` ((==) `on` bot)
      `also` (V.overlaps `on` arr)
    where
      also = (liftA2 . liftA2) (&&)

-- | See 'New'.
newBuffer :: Int -> IO Buffer
newBuffer n =
  Buffer
    <$> newIORef 0
    <*> newIORef 0
    <*> V.new (n + 1)

-- | See 'Put'.
putBuffer :: Int -> Buffer -> IO ()
putBuffer x Buffer{top, arr} = do
  i <- readIORef top
  V.write arr i x
  writeIORef top $! (i + 1) `mod` V.length arr

-- | See 'Get'.
getBuffer :: Buffer -> IO Int
getBuffer Buffer{bot, arr} = do
  j <- readIORef bot
  y <- V.read arr j
  writeIORef bot $! (j + 1) `mod` V.length arr
  return y

-- | See 'Len'.
lenBuffer :: Buffer -> IO Int
lenBuffer Buffer{top, bot, arr} = do
  i <- readIORef top
  j <- readIORef bot
  return $ (i - j) `mod` V.length arr

{-# LANGUAGE NamedFieldPuns #-}

-- | A Reasonably efficient version of a ring buffer.
-- Directly stolen from [q-s-m](https://github.com/stevana/quickcheck-state-machine/blob/master/test/CircularBuffer.hs#L86)
module Spec.CircularBuffer.Buffer where

import Data.Function (on)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Vector.Unboxed.Mutable (
  IOVector,
 )
import Data.Vector.Unboxed.Mutable qualified as V

-- | An efficient mutable circular buffer.
data Buffer = Buffer
  { size :: Int
  -- ^ Size of buffer
  , inp :: IORef Int
  -- ^ Index to the next free slot where to 'Put' the next element
  , outp :: IORef Int
  -- ^ Index to the last occupied slot to 'Get' an element from
  , buf :: IOVector Int
  -- ^ Array of elements of fixed capacity
  }

-- | Different buffers are assumed to have disjoint memories,
-- so we can use 'V.overlaps' to check equality.
instance Eq Buffer where
  (==) =
    ((==) `on` inp)
      `also` ((==) `on` outp)
      `also` (V.overlaps `on` buf)
    where
      also = (liftA2 . liftA2) (&&)

-- | See 'New'.
newBuffer :: Int -> IO Buffer
newBuffer n =
  Buffer size
    <$> newIORef 0
    <*> newIORef 0
    <*> V.new size
  where
    size = n + 1

-- | See 'Put'.
putBuffer :: Int -> Buffer -> IO ()
putBuffer x Buffer{size, inp, buf} = do
  i <- readIORef inp
  V.write buf i x
  writeIORef inp $! (i + 1) `mod` size

-- | See 'Get'.
getBuffer :: Buffer -> IO Int
getBuffer Buffer{size, outp, buf} = do
  j <- readIORef outp
  y <- V.read buf j
  writeIORef outp $! (j + 1) `mod` size
  return y

-- | See 'Len'.
lenBuffer :: Buffer -> IO Int
lenBuffer Buffer{inp, outp, size} = do
  i <- readIORef inp
  j <- readIORef outp
  pure $ (i - j + size) `rem` size

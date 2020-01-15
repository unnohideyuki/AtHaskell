module UnionFind where

import           Control.Monad
import           Control.Monad.Primitive
import qualified Data.Vector.Unboxed.Mutable as VM

newUF :: PrimMonad m => Int -> m (VM.MVector (PrimState m) Int)
newUF n = do
  d <- VM.new n
  VM.set d (-1)
  return d

findUF :: PrimMonad m => VM.MVector (PrimState m) Int -> Int -> m Int
findUF d x = do
  dx <- VM.read d x
  if dx < 0
    then return x
    else do dx' <- findUF d dx
            VM.write d x dx'
            return dx'

uniteUF :: PrimMonad m => VM.MVector (PrimState m) Int -> Int -> Int -> m ()
uniteUF d x y = do
  x' <- findUF d x
  y' <- findUF d y
  when (x' /= y') $ do
    let (x'', y'') = if (x' <= y') then (x', y') else (y', x')
    dx <- VM.read d x''
    dy <- VM.read d y''
    VM.write d x'' (dx + dy)
    VM.write d y'' x''
  return ()

sameUF :: PrimMonad m => VM.MVector (PrimState m) Int -> Int -> Int -> m Bool
sameUF d x y = do
  fx <- findUF d x
  fy <- findUF d y
  return $ fx == fy

sizeUF :: PrimMonad m => VM.MVector (PrimState m) Int -> Int -> m Int
sizeUF d x = do
  fx <- findUF d x
  dfx <- VM.read d fx
  return (-dfx)

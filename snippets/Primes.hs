module Primes where

import           Control.Monad
import qualified Control.Monad.ST            as ST
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as VM

-- generates a vector of prime numbers <= n
primes n = ST.runST $ do
  v <- VM.new (n+1)
  VM.write v 0 0
  VM.write v 1 0
  forM_ [2..n] $ \i -> VM.write v i i
  let sqrtN = ceiling $ sqrt $ fromIntegral n
  forM_ [2..sqrtN] $ \i -> do
    x <- VM.read v i
    when (x >= 0) (forM_ [2..(n`div`i)] $ \j -> VM.write v (i*j) 0)
  v' <- V.freeze v
  return $ V.filter (> 0) v'


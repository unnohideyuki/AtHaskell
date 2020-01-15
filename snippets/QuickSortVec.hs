{-# LANGUAGE BangPatterns #-}
module QuickSort where

import           Control.Monad
import           Control.Monad.Primitive
import qualified Control.Monad.ST            as ST
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as VM


-- quickSortVec from https://gist.github.com/kazu-yamamoto/3051375
quickSortList :: (Ord a, VM.Unbox a) => [a] -> [a]
quickSortList xs = ST.runST $ do
    arr <- V.thaw $ V.fromList xs
    let beg = 0
        end = VM.length arr - 1
    quickSortVec beg end arr
    V.toList <$> V.freeze arr

quickSortVec l u arr
  | l >= u    = return ()
  | otherwise = do
      VM.unsafeSwap arr l ((u+l)`div`2)
      t <- VM.unsafeRead arr l
      let i = l
          j = u + 1
      nj <- loopVec t u i j arr
      VM.unsafeSwap arr l nj
      quickSortVec l (nj-1) arr
      quickSortVec (nj+1) u arr

loopVec !t !u !i !j arr = do
  ni <- doWhile i (+1)         (< t)
  nj <- doWhile j (subtract 1) (> t)
  if ni > nj
    then return nj
    else do VM.unsafeSwap arr ni nj
            loopVec t u ni nj arr
              where
                {-# INLINE doWhile #-}
                doWhile k op p
                  | nk > u    = return nk
                  | otherwise = do
                      x <- VM.unsafeRead arr nk
                      if p x then
                        doWhile nk op p
                        else
                        return nk
                        where
                          nk = op k

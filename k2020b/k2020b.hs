{-# LANGUAGE BangPatterns #-}
import           Control.Monad
import           Control.Monad.Primitive
import qualified Control.Monad.ST            as ST
import qualified Data.ByteString.Char8       as BS
import           Data.Maybe
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as VM

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

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words
getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

main = do
  n <- getInt
  xs <- replicateM n $ do
    [x, l] <- getIntList
    return (x + l, x - l)

  let inf = 10^10 :: Int

  print $ sched 0 (-inf) (quickSortList xs)
  where
    sched ans _ [] = ans
    sched ans r ((e, b):xs) | r <= b = sched (ans+1) e xs
                            | otherwise = sched ans r xs

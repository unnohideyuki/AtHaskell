{-# LANGUAGE BangPatterns #-}
import           Control.Exception           (assert)
import           Control.Monad
import           Control.Monad.Primitive
import qualified Control.Monad.ST            as ST
import qualified Data.Array.IO               as IO
import qualified Data.Array.ST               as ST
import qualified Data.Array.Unboxed          as A
import           Data.Bits
import qualified Data.ByteString.Char8       as BS
import           Data.Char
import           Data.Foldable
import           Data.List
import           Data.Maybe
import qualified Data.Sequence               as Seq
import qualified Data.Set                    as Set
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as VM
import           Debug.Trace

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words
getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine
getIntVec n = V.unfoldrN n (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

readInteger = fst . fromJust . BS.readInteger
readIntegerList = map readInteger . BS.words
getInteger = readInteger <$> BS.getLine
getIntegerList = readIntegerList <$> BS.getLine

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

main = do
  [n, m] <- getIntList

  paths <- VM.new m
  forM_ [1..m] $ \i -> do
    [a, b, c] <- getIntList
    VM.write paths (i-1) (c, i, a-1, b-1)

  quickSortVec 0 (m-1) paths

  uf <- newUF n

  let solve j res
        | j < 0 = return res
        | otherwise = do
            (c, i, x, y) <- VM.read paths j
            same <- sameUF uf x y
            unless same (uniteUF uf x y)
            let res' = if same then res else res Seq.|> i
            solve (j-1) res'

  res <- solve (m-1) (Seq.empty)
  mapM_ print (Seq.unstableSort res)

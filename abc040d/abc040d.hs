{-# LANGUAGE BangPatterns #-}
import           Control.Monad
import           Control.Monad.Primitive
import qualified Control.Monad.ST            as ST
import qualified Data.ByteString.Char8       as BS
import           Data.List
import           Data.Maybe
import qualified Data.Vector.Unboxed         as V
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

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words
getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

-- quickSortVec from https://gist.github.com/kazu-yamamoto/3051375
-- quickSortVec :: (Ord a, Unbox a) => [a] -> [a]
quickSortList xs = ST.runST $ do
  arr <- V.unsafeThaw $ V.fromList xs
  let beg = 0
      end = VM.length arr - 1
  quickSortVec beg end arr
  V.toList <$> V.unsafeFreeze arr

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
  t <- replicateM m $ do [a, b, y] <- getIntList
                         return ((-y), (a, b))
  let yabs = quickSortList t

  q <- getInt
  t' <- forM [0..(q-1)] $ \i -> do [v, w] <- getIntList
                                   return ((-w), (i, v))
  let wvs = quickSortList t'

  ans <- VM.new q
  uf <- newUF (n+1)

  solve ans uf (yabs ++ [(0, (0, 0))]) wvs

  answers <- V.freeze ans
  V.forM_ answers $ \i -> print i
  where
    solve _ _ _ [] = return ()
    solve ans uf yabs@((y, (a, b)):yabs') wvs@((w, (i, v)):wvs') =
      if y < w
      then do uniteUF uf a b
              solve ans uf yabs' wvs
      else do sz <- sizeUF uf v
              VM.write ans i sz
              solve ans uf yabs wvs'

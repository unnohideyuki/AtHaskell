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


main = do
  [n, m] <- getIntList
  t <- replicateM m $ do [a, b, y] <- getIntList
                         return ((-y), (a, b))
  let yabs = sortOn fst t

  q <- getInt
  t' <- forM [0..(q-1)] $ \i -> do [v, w] <- getIntList
                                   return ((-w), (i, v))
  let wvs = sortOn fst t'

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

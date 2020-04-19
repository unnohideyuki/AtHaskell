import           Control.Exception     (assert)
import           Control.Monad
import qualified Control.Monad.ST      as ST
import qualified Data.Array.IO         as IO
import qualified Data.Array.Unboxed    as A
import           Data.Array.Unsafe
import qualified Data.ByteString.Char8 as BS
import           Data.List
import           Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words
getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

warshallFloyd d l r = do
  forM_ [l..r] $ \k ->
    forM_ [l..r] $ \i ->
      forM_ [l..r] $ \j -> do
        dik <- IO.readArray d (i,k)
        dkj <- IO.readArray d (k,j)
        dij <- IO.readArray d (i,j)
        when (dik + dkj < dij) $
          IO.writeArray d (i,j) (dik + dkj)

main = do
  let infty = 10^6 :: Int
  [n, m] <- getIntList

  arr <- IO.newArray ((1,1),(n,n)) infty :: IO (IO.IOUArray (Int, Int) Int)
  forM_ [1..n] $ \i -> IO.writeArray arr (i,i) 0
  replicateM_ m $ do
    [a, b, t] <- getIntList
    IO.writeArray arr (a, b) t
    IO.writeArray arr (b, a) t

  warshallFloyd arr 1 n

  d <- unsafeFreeze arr :: IO (A.UArray (Int, Int) Int)
  print $ minimum [maximum [d A.! (i, j) | j<-[1..n]] | i<-[1..n]]

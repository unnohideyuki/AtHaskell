import           Control.Exception     (assert)
import           Control.Monad
import qualified Control.Monad.ST      as ST
import qualified Data.Array.IO         as IO
import qualified Data.Array.ST         as ST
import qualified Data.Array.Unboxed    as A
import qualified Data.ByteString.Char8 as BS
import           Data.List
import           Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words
getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

warshallFloyd :: A.UArray (Int, Int) Int -> A.UArray (Int, Int) Int
warshallFloyd iarr = oarr
  where
    l  = (fst . fst . A.bounds) iarr
    l' = (snd . fst . A.bounds) iarr
    r  = (fst . snd . A.bounds) iarr
    r' = (snd . snd . A.bounds) iarr
    oarr = assert (l==l' && r==r') $ ST.runSTUArray $ do
      d <- ST.thaw iarr
      forM_ [l..r] $ \k ->
        forM_ [l..r] $ \i ->
          forM_ [l..r] $ \j -> do
            dik <- ST.readArray d (i,k)
            dkj <- ST.readArray d (k,j)
            dij <- ST.readArray d (i,j)
            when (dik + dkj < dij) $
              ST.writeArray d (i,j) (dik + dkj)
      return d

main = do
  let infty = 10^6 :: Int
  [n, m] <- getIntList

  arr <- IO.newArray ((1,1),(n,n)) infty :: IO (IO.IOUArray (Int, Int) Int)
  forM_ [1..n] $ \i -> IO.writeArray arr (i,i) 0
  replicateM_ m $ do
    [a, b, t] <- getIntList
    IO.writeArray arr (a, b) t
    IO.writeArray arr (b, a) t

  edges <- IO.freeze arr
  let d = warshallFloyd edges
  print $ minimum [maximum [d A.! (i, j) | j<-[1..n]] | i<-[1..n]]

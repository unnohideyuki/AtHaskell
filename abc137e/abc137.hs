import           Control.Exception           (assert)
import           Control.Monad
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

type Edge = (Int, Int, Int)
eFrm  (frm, _, _)  = frm
eTo   (_, to, _)   = to
eCost (_, _, cost) = cost

longestPath :: Int -> V.Vector (Int, Int, Int) -> Int
  -> IO (V.Vector Int)
longestPath s edges numV = do
  let numE = V.length edges
      inf = 10^18 :: Int

  d <- VM.new numV
  VM.set d (-inf)
  VM.write d s 0

  let loop :: Int -> IO ()
      loop i | i > 2*numV = return ()
             | otherwise = do update <- loop2 (i >= numV) 0 False
                              if update
                                then loop (i+1)
                                else return ()

      loop2 :: Bool -> Int -> Bool -> IO Bool
      loop2 cyc j upd
        | j == V.length edges = return upd
        | otherwise = do
            let e = edges V.! j
            d_frm <- VM.read d (eFrm e)
            d_to  <- VM.read d (eTo e)
            let ucond = d_frm /= (-inf) && d_to < d_frm + eCost e
                upd' = upd || ucond
            when (ucond && not cyc) $
              VM.write d (eTo e) (min inf (d_frm + eCost e))
            when (ucond && cyc) $ VM.write d (eTo e) inf
            loop2 cyc (j+1) upd'

  loop 0
  res <- V.freeze d
  return res

main = do
  [n, m, p] <- getIntList
  edges <- V.replicateM m $ do
    [a, b, c] <- getIntList
    return (a-1, b-1, c-p)

  costs <- longestPath 0 edges n

  let ans = costs V.! (n-1)
      inf = 10^18 :: Int

  print $ if ans == inf then (-1) else (max ans 0)

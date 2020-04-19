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
import qualified Data.Vector                 as VB
import qualified Data.Vector.Mutable         as VBM
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

-- (node, dist)
type Graph_dijk = VB.Vector (V.Vector (Int, Int))

dijkstra :: Int -> Graph_dijk -> Int -> IO (V.Vector Int)
dijkstra s g numV = do

  let inf = 10^9 :: Int
  d <- VM.new numV
  VM.set d inf
  VM.write d s 0

  -- tuple represent (min distance, node id)
  let set = Set.singleton (0, s)

      loop :: Set.Set (Int, Int) -> IO ()
      loop set | Set.null set = return ()
               | otherwise = do
                   let ((mdist, v), set') = Set.deleteFindMin set
                       edges = g VB.! v
                   d_v <- VM.read d v
                   if d_v < mdist
                     then loop set'
                     else do set'' <- loop2 v edges 0 set'
                             loop set''

      loop2 :: Int -> V.Vector (Int, Int) -> Int -> Set.Set (Int, Int)
            -> IO (Set.Set (Int, Int))
      loop2 v edges j set
        | j >= V.length edges = return set
        | otherwise = do
            let e = edges V.! j
                v_to = fst e
                cost = snd e
            d_v <- VM.read d v
            d_to <- VM.read d v_to
            let cond = d_to > d_v + cost
                set' = if cond
                       then (d_v + cost, v_to) `Set.insert` set
                       else set
            when cond $ VM.write d v_to (d_v + cost)
            loop2 v edges (j+1) set'

  loop set
  V.freeze d

main = do
  [n, m, t] <- getIntList
  as <- getIntVec n

  g <- VBM.new n
  g_rev <- VBM.new n
  VBM.set g ([] :: [(Int, Int)])
  VBM.set g_rev ([] :: [(Int, Int)])

  replicateM m $ do
    [a, b, c] <- getIntList
    let (a', b') = (a-1, b-1)
    xs <- VBM.read g a'
    ys <- VBM.read g_rev b'
    VBM.write g a' ((b', c):xs)
    VBM.write g_rev b' ((a', c):ys)

  g' <- VB.freeze g
  g_rev' <- VB.freeze g_rev

  let g'' = VB.map V.fromList g'
      g_rev'' = VB.map V.fromList g_rev'

  costs <- dijkstra 0 g'' n
  costs_r <- dijkstra 0 g_rev'' n

  let solve i r | i >= n = r
                | otherwise =
                    let waste = (costs V.! i) + (costs_r V.! i)
                        rest = max 0 (t - waste)
                        gain = rest * as V.! i
                    in solve (i+1) (max r gain)

  print $ solve 0 0

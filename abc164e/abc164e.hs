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

inf :: Int
inf = 10^18

-- (node, dist)
type Graph_dijk = VB.Vector (V.Vector (Int, Int))

dijkstra :: Int -> Graph_dijk -> Int -> IO (V.Vector Int)
dijkstra s g numV = do

  let inf = 10^18 :: Int
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
  let max_cities = 50
      max_silvers = 50 * 49
      fnode city silver = silver * 50 + city
      max_nodes = fnode max_cities (50*50)


  g <- VBM.new max_nodes
  VBM.set g (V.empty)

  [n, m, s0] <- getIntList

  replicateM_ m $ do
    [u, v, a, b] <- getIntList
    forM_ [a..max_silvers] $ \s -> do
      let n_frm1 = fnode (u-1) s
          n_to1  = fnode (v-1) (s-a)
          n_frm2 = fnode (v-1) s
          n_to2  = fnode (u-1) (s-a)
      t1 <- VBM.read g n_frm1
      VBM.write g n_frm1 (t1 `V.snoc` (n_to1, b))
      t2 <- VBM.read g n_frm2
      VBM.write g n_frm2 (t2 `V.snoc` (n_to2, b))

  forM_ [0..(n-1)] $ \i -> do
    [c, d] <- getIntList
    forM_ [0..(max_silvers - 1)] $ \s -> do
      let n_frm = fnode i s
          n_to  = fnode i (min max_silvers (s+c))
      t <- VBM.read g n_frm
      VBM.write g n_frm (t `V.snoc` (n_to, d))

  g' <- VB.freeze g

  let n_start = fnode 0 (min max_silvers s0)
  d <- dijkstra n_start g' max_nodes

  let solve i j res | j > max_silvers = res
                    | otherwise =
                        let x = d V.! fnode i j
                        in solve i (j+1) (min x res)

  forM_ [1..(n-1)] $ \i -> do
    let a = solve i 0 inf
    print a

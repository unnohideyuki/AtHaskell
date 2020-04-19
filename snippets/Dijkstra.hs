module Dijkstra where

import           Control.Monad
import qualified Data.Array.IO               as IO
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

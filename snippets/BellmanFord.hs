module BellmanFord where

import           Control.Monad
import qualified Data.Array.IO               as IO
import           Data.Bits
import qualified Data.ByteString.Char8       as BS
import           Data.Char
import           Data.Foldable
import           Data.List
import           Data.Maybe
import qualified Data.Sequence               as Seq
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as VM

type Edge = (Int, Int, Int)
eFrm  (frm, _, _)  = frm
eTo   (_, to, _)   = to
eCost (_, _, cost) = cost

shortestPath s edges numV = do
  let numE = V.length edges
      inf = 10^9 :: Int

  d <- VM.new numV
  VM.set d inf
  VM.write d s 0

  let loop :: Int -> IO Bool
      loop i | i == numV = return True -- cyclic
             | otherwise = do update <- loop2 0 False
                              if update
                                then loop (i+1)
                                else return False

      loop2 :: Int -> Bool -> IO Bool
      loop2 j upd | j == V.length edges = return upd
                  | otherwise = do
                      let e = edges V.! j
                      d_frm <- VM.read d (eFrm e)
                      d_to  <- VM.read d (eTo e)
                      let ucond = d_frm /= inf && d_to > d_frm + eCost e
                          upd' = upd || ucond
                      when ucond $ VM.write d (eTo e) (d_frm + eCost e)
                      loop2 (j+1) upd'

  cyc <- loop 0
  res <- V.freeze d

  if cyc
    then return Nothing
    else return (Just res)

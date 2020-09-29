{-# LANGUAGE DatatypeContexts #-}
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
import qualified Data.Map.Strict             as Map
import           Data.Maybe
import qualified Data.Sequence               as Seq
import qualified Data.Set                    as Set
import qualified Data.Vector                 as VB
import qualified Data.Vector.Mutable         as VBM
import qualified Data.Vector.Unboxed         as V
import           Data.Vector.Unboxed.Base
import qualified Data.Vector.Unboxed.Mutable as VM
import           Debug.Trace

import           Segtree

main = do
  let inf = 10^9 :: Int
  seg <- fromVecSEGT min inf (V.fromList [5, 3, 7, 9, 6, 4, 1, 2])

  s <- showSEGT seg
  putStrLn s

  min05 <- prodSEGT seg 0 6
  print min05

  min_all <- all_prodSEGT seg
  print min_all

  min24 <- prodSEGT seg 2 5
  print min24

  min23 <- prodSEGT seg 2 4
  print min23




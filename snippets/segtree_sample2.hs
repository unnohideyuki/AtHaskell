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

main = do
  [n, k] <- getIntList

  seg <- newSEGT max (0::Int) 300010
  ans <- VM.new 1
  VM.set ans (0::Int)

  replicateM_ n $ do
    x <- getInt
    let l = max (x - k) 0
        r = min (x + k) 300000
    tmp <- prodSEGT seg l (r+1)
    a <- VM.read ans 0
    let tmp' = tmp + 1
        a' = max a tmp'
    print tmp'
    VM.write ans 0 a'
    setSEGT seg x tmp'

  a <- VM.read ans 0
  print a


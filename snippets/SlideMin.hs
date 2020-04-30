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
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as VM
import           Debug.Trace


-- デックの利用　スライド最小値
-- 蟻本 p.300

slideMin :: V.Vector Int -> Int -> Seq.Seq Int
slideMin as k = let dq = loopAppend 0 Seq.empty
                in loopBs (k-1) dq Seq.empty
  where n = V.length as

        appendDq dq i | Seq.null dq = dq Seq.|> i
                      | otherwise = let dq' Seq.:> r = Seq.viewr dq
                                    in if r >= as V.! i
                                       then appendDq dq' i
                                       else dq Seq.|> i

        loopAppend i dq | i == k - 1 = dq
                        | otherwise = loopAppend (i+1) (appendDq dq i)

        loopBs i dq bs | i == n = bs
                       | otherwise = let dq' = appendDq dq i
                                         l Seq.:< dq'' = Seq.viewl dq'
                                         bs' = bs Seq.|> (as V.! l)
                                         j = i - k + 1
                                         dq''' = if l == j
                                                 then dq''
                                                 else dq'
                                     in loopBs (i+1) dq''' bs'


--- sample
main = do
  let k = 3
      as = V.fromList [1, 3 ,5, 4, 2]
  print $ slideMin as k

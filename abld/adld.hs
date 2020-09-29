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

data (PrimMonad m, Unbox a, Show a) => Segtree m a =
  Segtree { segtree_n    :: Int
          , segtree_size :: Int
          , segtree_log  :: Int
          , segtree_d    :: VM.MVector (PrimState m) a
          , segtree_op   :: a -> a -> a
          , segtree_e    :: a
          }

newSEGT :: (PrimMonad m, Unbox a, Show a) => (a -> a -> a) -> a -> Int -> m (Segtree m a)
newSEGT op e n = do
  let v = V.replicate n e
  fromVecSEGT op e v

fromVecSEGT :: (PrimMonad m, Unbox a, Show a) => (a -> a -> a) -> a -> (V.Vector a) -> m (Segtree m a)
fromVecSEGT op e v = do
  let _n = V.length v
      _log = (ceiling $ logBase 2 (fromIntegral _n)) :: Int
      _size = 1 `shiftL` _log
  _d <- VM.new (2 * _size)
  VM.set _d e
  forM_ [0 .. (_n - 1)] $ \i -> VM.write _d (_size + i) (v V.! i)

  let segtree = Segtree { segtree_n = _n
                        , segtree_size = _size
                        , segtree_log = _log
                        , segtree_d = _d
                        , segtree_op = op
                        , segtree_e = e
                        }

  forM_ [(_size - 1),(_size -2) .. 1] $ \i -> updateSEGT segtree i
  return segtree

setSEGT :: (PrimMonad m, Unbox a, Show a) => (Segtree m a) -> Int -> a -> m ()
setSEGT segtree p x = do
  let _n = segtree_n segtree
      _size = segtree_size segtree
      _log = segtree_log segtree
      _d = segtree_d segtree
  assert (0 <= p && p < _n) $ do
    let p' = p + _size
    VM.write _d p' x
    forM_ [1 .. _log] $ \i -> updateSEGT segtree (p' `shiftR` i)

getSEGT :: (PrimMonad m, Unbox a, Show a) => (Segtree m a) -> Int -> m a
getSEGT segtree p = do
  let _n = segtree_n segtree
      _size = segtree_size segtree
      _d = segtree_d segtree
  assert (0 <= p && p < _n) $ do
    VM.read _d (p + _size)

prodSEGT :: (PrimMonad m, Unbox a, Show a) => (Segtree m a) -> Int -> Int -> m a
prodSEGT segtree l r = do
  let _n = segtree_n segtree
      _size = segtree_size segtree
      _log = segtree_log segtree
      _d = segtree_d segtree
      _op = segtree_op segtree
      _e = segtree_e segtree
  assert (0 <= l && l <= r && r <= _n) $ do
    let prod_loop l r sml smr
          | l < r = do dl <- VM.read _d l
                       dr <- VM.read _d (r-1)
                       let sml' | (l .&. 1) == 1 = _op sml dl
                                | otherwise = sml
                           smr' | (r .&. 1) == 1 = _op dr smr
                                | otherwise = smr
                           l' = l + (l .&. 1)
                           r' = r - (r .&. 1)
                       prod_loop (l' `shiftR` 1) (r' `shiftR` 1) sml' smr'
          | otherwise = return $ _op sml smr
    prod_loop (l + _size) (r + _size) _e _e

all_prodSEGT :: (PrimMonad m, Unbox a, Show a) => (Segtree m a) -> m a
all_prodSEGT segtree = do
  let _d = segtree_d segtree
  VM.read _d 1

updateSEGT :: (PrimMonad m, Unbox a, Show a) => (Segtree m a) -> Int -> m ()
updateSEGT segtree k = do
  let _d = segtree_d segtree
      _op = segtree_op segtree
  x <- VM.read _d (2*k)
  y <- VM.read _d (2*k + 1)
  VM.write _d k (_op x y)

showSEGT :: (PrimMonad m, Unbox a, Show a) => (Segtree m a) -> m String
showSEGT segtree = do
  let _n = segtree_n segtree
      _size = segtree_size segtree
      _log = segtree_log segtree
      _d = segtree_d segtree
  d' <- V.freeze _d
  let s =
        "Segtree{ _n=" ++ show _n
        ++ ", _size=" ++ show _size
        ++ ", _log=" ++ show _log
        ++ ", _d=" ++ show d'
        ++ "}"
  return s


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
    VM.write ans 0 a'
    setSEGT seg x tmp'

  a <- VM.read ans 0
  print a

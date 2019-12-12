import           Control.Exception  (assert)
import           Control.Monad
import qualified Control.Monad.ST   as ST
import qualified Data.Array.ST      as ST
import qualified Data.Array.Unboxed as A
import           Data.Maybe

warshallFloyd :: A.UArray (Int, Int) Int -> A.UArray (Int, Int) Int
warshallFloyd iarr = oarr
  where
    l  = (fst . fst . A.bounds) iarr
    l' = (snd . fst . A.bounds) iarr
    r  = (fst . snd . A.bounds) iarr
    r' = (snd . snd . A.bounds) iarr
    oarr = assert (l==l' && r==r') $ ST.runSTUArray $ do
      d <- ST.thaw iarr
      forM_ [l..r] $ \k -> do
        forM_ [l..r] $ \i -> do
          forM_ [l..r] $ \j -> do
            dik <- ST.readArray d (i,k)
            dkj <- ST.readArray d (k,j)
            dij <- ST.readArray d (i,j)
            when (dik + dkj < dij) $
              ST.writeArray d (i,j) (dik + dkj)
      return d

main = do
  let infty = 10^6 :: Int
      graph =  [((1,1), 0), ((2,2), 0), ((3,3), 0),
                 ((4,4), 0), ((5,5), 0), ((6,6), 0),
                 ((2,1), 1), ((2,3), 1), ((3,4), 1),
                 ((3,6), 1), ((4,3), 1), ((5,4), 1),
                 ((5,2), 1), ((6,1), 1)]
      arr = ST.runSTUArray $ do
        marr <- ST.newArray ((1,1),(6,6)) infty
        forM_ graph $ \(i,v) ->
          ST.writeArray marr i v
        return marr
  print $ A.assocs $ warshallFloyd arr



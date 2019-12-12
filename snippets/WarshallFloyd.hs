import           Control.Monad
import qualified Control.Monad.ST   as ST
import qualified Data.Array.ST      as ST
import qualified Data.Array.Unboxed as A
import           Data.Maybe

{-
    public static void WarshallFloyd(int[,] d, int v)
    {
        for (int k = 0; k < v; k++)
            for (int i = 0; i < v; i++)
                for (int j = 0; j < v; j++)
                    d[i, j] = Math.Min(d[i, j], d[i, k] + d[k, j]);
    }
-}

warshallFloyd ::
  A.UArray (Int, Int) Int -> Int -> A.UArray (Int, Int) Int
warshallFloyd iarr n = oarr
  where
    oarr = ST.runSTUArray $ do
      d <- ST.thaw iarr
      forM_ [1..n] $ \k -> do
        forM_ [1..n] $ \i -> do
          forM_ [1..n] $ \j -> do
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
  print $ A.assocs $ warshallFloyd arr 6



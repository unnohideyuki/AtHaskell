import           Control.Monad
import           Data.Bits
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe
import qualified Data.Vector           as BV

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words
getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

main = do
  n <- getInt
  tss <- BV.replicateM n $ do
    m <- getInt
    replicateM m $ do
      [x, y] <- getIntList
      return (x-1, y)

  let
    solve :: Int -> Int -> Int
    solve x y | x == 0    = y
              | otherwise = solve (x - 1) (max (solve' n x 0) y)
    solve' i x m | i == -1 = m
                 | otherwise =
                   let
                     c = check i x
                   in
                     if c >= 0 then solve' (i-1) x (m+c) else -1
    check i x = if testBit x i
      then if and $ map (\(a, b) -> testBit x a == (b == 1)) (tss BV.! i)
           then 1
           else -1
      else 0

  print $ solve ((1 `shift` n) - 1) 0





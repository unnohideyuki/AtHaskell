import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words
getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

bisectRight :: Ord a => (Int -> a) -> a -> Int -> Int -> Int
bisectRight f x lo hi
  | lo >= hi  = lo
  | x < f mid = bisectRight f x lo mid
  | otherwise = bisectRight f x (mid+1) hi
  where
    mid = lo + (hi - lo) `div` 2

main = do
  [a, b, x] <- getIntList
  let price n = a*n + b * (length.show) n
  print $ bisectRight price x 1 1000000001 - 1

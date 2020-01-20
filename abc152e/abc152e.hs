import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.Char
import           Data.Maybe
import qualified Data.Vector           as VB

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words
getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

getIntegerVec n =
  VB.unfoldrN n (BS.readInteger . BS.dropWhile isSpace) <$> BS.getLine

main = do
  n <- getInt
  as <- getIntegerVec n
  let m = VB.foldl' lcm 1 as
      as' = VB.map (\i -> m `div` i) as
  print $ (VB.sum as') `mod` 1000000007

import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words
getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

main = do
  n <- getInt
  (x:y:s) <- getLine
  let t = zip3 (x:y:s) (y:s) s
      u = map (\(a, b, c) -> if [a, b, c] == "ABC" then 1 else 0) t
  print $ sum u

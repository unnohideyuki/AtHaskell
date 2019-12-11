import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words
getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

main = do
  as <- getIntList
  putStrLn $ if sum as >= 22 then "bust" else "win"

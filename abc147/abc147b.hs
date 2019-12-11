import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe

main = do
  s <- BS.getLine
  let s' = BS.reverse s
      d = sum $ map (\(x, y) -> if x == y then 0 else 1) $ BS.zip s s'
  print $ d `div` 2


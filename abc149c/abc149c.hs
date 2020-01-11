import           Control.Monad
import qualified Control.Monad.ST            as ST
import qualified Data.ByteString.Char8       as BS
import           Data.List
import           Data.Maybe
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as VM

readInt = fst . fromJust . BS.readInt
getInt = readInt <$> BS.getLine

primes n = ST.runST $ do
  v <- VM.new (n+1)
  VM.write v 0 0
  VM.write v 1 0
  forM_ [2..n] $ \i -> VM.write v i i
  let sqrtN = ceiling $ sqrt $ fromIntegral n
  forM_ [2..sqrtN] $ \i -> do
    x <- VM.read v i
    when (x > 0) (forM_ [2..(n`div`i)] $ \j -> VM.write v (i*j) 0)
  v' <- V.freeze v
  return $ V.filter (> 0) v'

main = do
  x <- getInt
  print $ V.head $ V.dropWhile (< x) $ primes 100003

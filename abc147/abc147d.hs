import           Control.Monad
import           Data.Array.IO
import           Data.Bits
import qualified Data.ByteString.Char8       as BS
import           Data.Char
import           Data.Maybe
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as VM

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words
getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine
getIntVec n = V.unfoldrN n (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

main = do
  n <- getInt
  as <- getIntVec n

  cs <- VM.new 61
  forM_ [0..60] $ \i -> VM.write cs i (0::Int)

  forM_ [0..n-1] $ \i -> do
    let a = as V.! i
    forM_ [0..60] $ \j ->
      when (testBit a j) $ do
        c <- VM.read cs j
        VM.write cs j (c+1)

  let calc i x | i == -1 = return $ x `mod` 1000000007
               | otherwise = do
                   c <- VM.read cs i
                   let c1 = fromIntegral c :: Integer
                       c2 = fromIntegral (n-c) :: Integer
                       x' = c1 * c2 * bit i
                   calc (i-1) (x+x')
  ans <- calc 60 0
  print ans

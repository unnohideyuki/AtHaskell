import           Control.Monad
import           Data.Array.IO
import           Data.Bits
import qualified Data.ByteString.Char8       as BS
import           Data.Char
import           Data.Maybe
import qualified Data.Vector                 as VB
import qualified Data.Vector.Mutable         as VBM
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as VM
import           System.IO

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words
getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

getEdges es = do
  e <- getIntList
  eof <- isEOF
  if eof
    then return (e:es)
    else getEdges (e:es)

main = do
  n <- getInt
  es <- getEdges []

  vs <- VBM.new (n+1)
  VBM.set vs []

  forM_ es $ \[u, v, w] -> do
    tu <- VBM.read vs u
    VBM.write vs u ((v, w):tu)
    tv <- VBM.read vs v
    VBM.write vs v ((u, w):tv)

  vs' <- VB.freeze vs

  ds <- VM.new (n+1)
  VM.set ds (-1::Int)

  let dfs i d = do
        di <- VM.read ds i
        if di >= 0
          then return ()
          else do VM.write ds i d
                  forM_ (vs' VB.! i) $ \(j, w) -> dfs j (d+w)

  dfs 1 0

  forM_ [1..n] $ \i -> do
    when (i > 1) $ putStr " "
    di <- VM.read ds i
    putStr . show $ di .&. 1

  putStrLn ""


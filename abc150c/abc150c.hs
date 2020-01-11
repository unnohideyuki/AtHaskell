import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words
getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

nextPermutation :: Ord a => [a] -> Maybe [a]
nextPermutation xs =
  let
    len = length xs
    -- Reverse of longest non-increasing suffix
    revSuffix = findPrefix (reverse xs)
    suffixLen = length revSuffix
    prefixMinusPivot = take (len - suffixLen - 1) xs
    pivot = xs !! (len - suffixLen - 1)
    suffixHead = takeWhile (<= pivot) revSuffix
    newPivot : suffixTail = drop (length suffixHead) revSuffix
    newSuffix = suffixHead ++ (pivot : suffixTail)
  in
    if suffixLen == len
    then Nothing
    else Just (prefixMinusPivot ++ (newPivot : newSuffix))
  where
    findPrefix [] = []
    findPrefix (x:xs) =
      x : (if xs /= [] && x <= head xs then findPrefix xs else [])

main = do
  _ <- getInt
  [p', q'] <- replicateM 2 getIntList
  let (p, q) = if p' <= q' then (p', q') else (q', p')
  print $ solve p q 0
  where
    solve p q m | p == q = m
                | otherwise = let p' = (fromJust . nextPermutation) p
                              in solve p' q (m + 1)

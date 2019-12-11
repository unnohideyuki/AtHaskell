bisectRight :: Ord a => (Int -> a) -> a -> Int -> Int -> Int
bisectRight f x lo hi
  | lo >= hi  = lo
  | x < f mid = bisectRight f x lo mid
  | otherwise = bisectRight f x (mid+1) hi
  where
    mid = lo + (hi - lo) `div` 2

bisectLeft :: Ord a => (Int -> a) -> a -> Int -> Int -> Int
bisectLeft f x lo hi
  | lo >= hi  = lo
  | f mid < x = bisectLeft f x (mid+1) hi
  | otherwise = bisectLeft f x lo mid
  where
    mid = lo + (hi - lo) `div` 2

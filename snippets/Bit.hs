{-# LANGUAGE BangPatterns     #-}
{- BIT: Binary Indexed Tree or Fenwick Tree -}
data BIT = BIT { bit_size :: !Int
               , bit_d    :: VM.MVector (PrimState IO) Int
               }

-- create a BIT with size n
newBIT :: Int -> IO BIT
newBIT n = do d <- VM.new n
              VM.set d (0::Int)
              return $ BIT n d

-- sum of the range [0, i] (0 <= i <= (n-1))
sumBIT :: BIT -> Int -> IO Int
sumBIT (BIT n d) i = loop (i+1) 0
  where loop :: Int -> Int -> IO Int
        loop i s | i <= 0 = return s
                 | otherwise = do t <- VM.read d (i-1)
                                  let s' = s + t
                                      i' = i - (i .&. (-i))
                                  loop i' s'

-- add x to bit[i] (0 <= i <= (n-1))
addBIT :: BIT -> Int -> Int -> IO ()
addBIT bit@(BIT n d) i x = loop (i+1)
  where loop :: Int -> IO ()
        loop i | i > n = return ()
               | otherwise = do t <- VM.read d (i-1)
                                VM.write d (i-1) (t+x)
                                let i' = i + (i .&. (-i))
                                loop i'


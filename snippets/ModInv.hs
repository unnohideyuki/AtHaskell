module ModInv where

import           Control.Monad
import           Data.Bits

-- compulte a^n `mod` p
modpow :: Int -> Int -> Int -> Int
modpow a n p =
  let modpow' a n res | n == 0    = res
                      | otherwise =
                          modpow' (a*a`mod`p)
                                  (n `shiftR` 1)
                                  (if ((n .&. 1) == 1) then res * a `mod` p else res)
  in modpow' a n 1


-- compute a^(-1) `mod` p
modinv :: Int -> Int -> Int
modinv a p = modpow a (p-2) p


main = do
  forM_ [1..13] $ \i -> (print $ modinv i 13)
  -- it should be 123456789123456789
  print $ 678813585 * (modinv 100000 1000000007) `mod` 1000000007


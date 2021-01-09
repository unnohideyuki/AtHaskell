-- compulte a^n `mod` p
modpow :: Int -> Int -> Int -> Int
modpow a n p =
  let modpow' :: Integer -> Integer -> Integer -> Integer
      modpow' a n res | n == 0    = res
                      | otherwise =
                          modpow' (a*a`mod`p')
                                  (n `shiftR` 1)
                                  (if (n .&. 1) == 1 then res * a `mod` p' else res)
      p' = fromIntegral p
  in fromInteger $ modpow' (fromIntegral a) (fromIntegral n) 1

-- compute a^(-1) `mod` p
modinv :: Int -> Int -> Int
modinv a p = modpow a (p-2) p

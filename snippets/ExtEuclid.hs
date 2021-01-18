{-# LANGUAGE BangPatterns #-}

-- extEuclid -- solves ax + by = gcd a b
extEuclid :: Int -> Int -> (Int, Int)
extEuclid !a !b | b == 0 = (1, 0)
                | otherwise = let (q, r) = a `divMod` b
                                  (s, t) = extEuclid b r
                              in (t, s - q*t)

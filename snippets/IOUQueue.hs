{-# LANGUAGE BangPatterns #-}

data IOUQueue a = IOUQueue { uq_max_size :: !Int
                           , uq_i        :: !Int
                           , uq_j        :: !Int
                           , uq_v        :: VM.MVector (PrimState IO) a
                           }

newUDeque :: (Unbox a) => Int -> IO (IOUQueue a)
newUDeque sz = do let k = ceiling $ logBase 2 (fromIntegral sz)
                      msz = (1 `shiftL` k) - 1
                  v <- VM.new msz
                  return $ IOUQueue msz 0 0 v

nullUQ :: (Unbox a) => IOUQueue a -> Bool
nullUQ uq@(IOUQueue _ i j _) = (i == j)

lengthUQ :: (Unbox a) => IOUQueue a -> Int
lengthUQ (IOUQueue msz i j _) | i <= j = j - i
                              | otherwise = (j + msz + 1) - i

popFrontUQ :: (Unbox a) => IOUQueue a -> IO (IOUQueue a, a)
popFrontUQ uq@(IOUQueue msz i j v) | lengthUQ uq > 0 = do r <- VM.unsafeRead v i
                                                          let i' = (i+1) .&. msz
                                                          return (uq{uq_i=i'}, r)
                                   | otherwise = error "popFrontUQ: empty queue"

popBackUQ :: (Unbox a) => IOUQueue a -> IO (IOUQueue a, a)
popBackUQ uq@(IOUQueue msz i j v) | lengthUQ uq > 0 = do let j' = (j-1) .&. msz
                                                         r <- VM.unsafeRead v j'
                                                         return (uq{uq_j=j'}, r)
                                  | otherwise = do error "popBackUQ: empty queue"

pushBackListUQ :: (Unbox a) => IOUQueue a -> [a] -> IO (IOUQueue a)
pushBackListUQ uq [] = return uq
pushBackListUQ uq@(IOUQueue msz i j v) (x:xs)
  | lengthUQ uq < msz = do VM.unsafeWrite v j x
                           let j' = (j+1) .&. msz
                           pushBackListUQ uq{uq_j=j'} xs
  | otherwise = error "pushBackListUQ: overflow"

pushBackListWithDUQ :: (Unbox a, Unbox b) => IOUQueue (a, b) -> [a] -> b -> IO (IOUQueue (a, b))
pushBackListWithDUQ uq [] _ = return uq
pushBackListWithDUQ uq@(IOUQueue msz i j v) (x:xs) d
  | lengthUQ uq < msz = do VM.unsafeWrite v j (x, d)
                           let j' = (j+1) .&. msz
                           pushBackListWithDUQ uq{uq_j=j'} xs d
  | otherwise = error "pushBackListWithDUQ: overflow"

pushBackUQ :: (Unbox a) => IOUQueue a -> a -> IO (IOUQueue a)
pushBackUQ uq@(IOUQueue msz i j v) x
  | lengthUQ uq < msz = do VM.unsafeWrite v j x
                           let j' = (j+1) .&. msz
                           return $ uq{uq_j=j'}
  | otherwise = error "pushBackUQ: overflow"

pushFrontListUQ :: (Unbox a) => IOUQueue a -> [a] -> IO (IOUQueue a)
pushFrontListUQ uq [] = return uq
pushFrontListUQ uq@(IOUQueue msz i j v) (x:xs)
  | lengthUQ uq < msz = do let i' = (i-1) .&. msz
                           VM.unsafeWrite v i' x
                           pushFrontListUQ uq{uq_i=i'} xs
  | otherwise = error "pushFrontListUQ: overflow"

pushFrontListWithDUQ :: (Unbox a, Unbox b) => IOUQueue (a, b) -> [a] -> b -> IO (IOUQueue (a, b))
pushFrontListWithDUQ uq [] _ = return uq
pushFrontListWithDUQ uq@(IOUQueue msz i j v) (x:xs) d
  | lengthUQ uq < msz = do let i' = (i-1) .&. msz
                           VM.unsafeWrite v i' (x, d)
                           pushFrontListWithDUQ uq{uq_i=i'} xs d
  | otherwise = error "pushFrontListWithDUQ: overflow"

pushFrontUQ :: (Unbox a) => IOUQueue a -> a -> IO (IOUQueue a)
pushFrontUQ uq@(IOUQueue msz i j v) x
  | lengthUQ uq < msz = do let i' = (i-1) .&. msz
                           VM.unsafeWrite v i' x
                           return $ uq{uq_i=i'}
  | otherwise = error "pushFrontUQ: overflow"


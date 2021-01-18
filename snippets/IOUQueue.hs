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

growUQ :: (Unbox a) => IOUQueue a -> IO (IOUQueue a)
growUQ (IOUQueue msz i j v) = do v' <- VM.grow v (msz+1)
                                 when (j < i) $ forM_ [0..(j-1)] $ \k -> do
                                   t <- VM.read v' k
                                   VM.write v' (k+msz+1) t
                                 let j' | j < i = j + msz + 1
                                        | otherwise = j
                                 return (IOUQueue (2*msz+1) i j' v')

pushBackListUQ :: (Unbox a) => IOUQueue a -> [a] -> IO (IOUQueue a)
pushBackListUQ uq [] = return uq
pushBackListUQ uq@(IOUQueue msz i j v) (x:xs)
  | lengthUQ uq < msz = do VM.unsafeWrite v j x
                           let j' = (j+1) .&. msz
                           pushBackListUQ uq{uq_j=j'} xs
  | otherwise = do uq' <- growUQ uq
                   pushBackListUQ uq' (x:xs)

pushBackListWithDUQ :: (Unbox a, Unbox b) => IOUQueue (a, b) -> [a] -> b -> IO (IOUQueue (a, b))
pushBackListWithDUQ uq [] _ = return uq
pushBackListWithDUQ uq@(IOUQueue msz i j v) (x:xs) d
  | lengthUQ uq < msz = do VM.unsafeWrite v j (x, d)
                           let j' = (j+1) .&. msz
                           pushBackListWithDUQ uq{uq_j=j'} xs d
  | otherwise = do uq' <- growUQ uq
                   pushBackListWithDUQ uq' (x:xs) d

pushBackUQ :: (Unbox a) => IOUQueue a -> a -> IO (IOUQueue a)
pushBackUQ uq@(IOUQueue msz i j v) x
  | lengthUQ uq < msz = do VM.unsafeWrite v j x
                           let j' = (j+1) .&. msz
                           return $ uq{uq_j=j'}
  | otherwise = do uq' <- growUQ uq
                   pushBackUQ uq' x

pushFrontListUQ :: (Unbox a) => IOUQueue a -> [a] -> IO (IOUQueue a)
pushFrontListUQ uq [] = return uq
pushFrontListUQ uq@(IOUQueue msz i j v) (x:xs)
  | lengthUQ uq < msz = do let i' = (i-1) .&. msz
                           VM.unsafeWrite v i' x
                           pushFrontListUQ uq{uq_i=i'} xs
  | otherwise = do uq' <- growUQ uq
                   pushFrontListUQ uq' (x:xs)

pushFrontListWithDUQ :: (Unbox a, Unbox b) => IOUQueue (a, b) -> [a] -> b -> IO (IOUQueue (a, b))
pushFrontListWithDUQ uq [] _ = return uq
pushFrontListWithDUQ uq@(IOUQueue msz i j v) (x:xs) d
  | lengthUQ uq < msz = do let i' = (i-1) .&. msz
                           VM.unsafeWrite v i' (x, d)
                           pushFrontListWithDUQ uq{uq_i=i'} xs d
  | otherwise = do uq' <- growUQ uq
                   pushFrontListWithDUQ uq' (x:xs) d

pushFrontUQ :: (Unbox a) => IOUQueue a -> a -> IO (IOUQueue a)
pushFrontUQ uq@(IOUQueue msz i j v) x
  | lengthUQ uq < msz = do let i' = (i-1) .&. msz
                           VM.unsafeWrite v i' x
                           return $ uq{uq_i=i'}
  | otherwise = do uq' <- growUQ uq
                   pushFrontUQ uq' x

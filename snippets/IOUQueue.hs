{-# LANGUAGE BangPatterns #-}

data IOUQueue a = IOUQueue { uq_size :: !Int
                           , uq_orig :: !Int
                           , uq_i    :: !Int
                           , uq_j    :: !Int
                           , uq_v    :: VM.MVector (PrimState IO) a
                           }

newUDequeue :: (Unbox a) => Int -> Int -> IO (IOUQueue a)
newUDequeue sizel sizer = do let sz = sizel + sizer
                             v <- VM.new sz
                             return $ IOUQueue sz sizel sizel sizel v

newUQueue :: (Unbox a) => Int -> IO (IOUQueue a)
newUQueue sz = newUDequeue 0 sz

nullUQ :: (Unbox a) => IOUQueue a -> Bool
nullUQ uq@(IOUQueue _ _ i j _) = (i == j)

poplUQ :: (Unbox a) => IOUQueue a -> IO (IOUQueue a, a)
poplUQ uq@(IOUQueue sz o i j v) | i < j = do r <- VM.unsafeRead v i
                                             return (uq{uq_i=i+1}, r)
                                | otherwise = error "poplUQ: empty queue"

popRUQ :: (Unbox a) => IOUQueue a -> IO (IOUQueue a, a)
popRUQ uq@(IOUQueue sz o i j v) | i < j = do r <- VM.unsafeRead v (j-1)
                                             return (uq{uq_j=j-1}, r)
                                | otherwise = error "poprUQ: empty queue"

recenterUQ :: (Unbox a) => IOUQueue a -> IO (IOUQueue a)
recenterUQ uq@(IOUQueue sz o i j v)
  | i == o = return uq
  | i > o = do let dist = i - o
               forM_ [i..(j-1)] $ \k -> do
                 t <- VM.unsafeRead v k
                 VM.unsafeWrite v (k-dist) t
               return $ uq{uq_i=i-dist, uq_j=j-dist}
  | otherwise = do let dist = o - j
                   forM_ [j-1,(j-2)..i] $ \k -> do
                     t <- VM.unsafeRead v k
                     VM.unsafeWrite v (k+dist) t
                   return $ uq{uq_i=i+dist, uq_j=j+dist}

pushListUQ :: (Unbox a) => IOUQueue a -> [a] -> IO (IOUQueue a)
pushListUQ uq [] = return uq
pushListUQ uq@(IOUQueue sz o i j v) (x:xs)
  | j < sz = do VM.unsafeWrite v j x
                pushListUQ uq{uq_j=j+1} xs
  | i > o  = do uq' <- recenterUQ uq
                pushListUQ uq' (x:xs)
  | otherwise = error "pushListUQ: overflow"

pushListWithDUQ :: (Unbox a, Unbox b) => IOUQueue (a, b) -> [a] -> b -> IO (IOUQueue (a, b))
pushListWithDUQ uq [] _ = return uq
pushListWithDUQ uq@(IOUQueue sz o i j v) (x:xs) d
  | j < sz = do VM.unsafeWrite v j (x, d)
                pushListWithDUQ uq{uq_j=j+1} xs d
  | i > o  = do uq' <- recenterUQ uq
                pushListWithDUQ uq' (x:xs) d
  | otherwise = error "pushListWithDUQ: overflow"

pushUQ :: (Unbox a) => IOUQueue a -> a -> IO (IOUQueue a)
pushUQ uq@(IOUQueue sz o i j v) x
  | j < sz = do VM.unsafeWrite v j x
                return $ uq{uq_j=j+1}
  | i > o = do uq' <- recenterUQ uq
               pushUQ uq' x
  | otherwise = error "pushUQ: overflow"

pushLListUQ :: (Unbox a) => IOUQueue a -> [a] -> IO (IOUQueue a)
pushLListUQ uq [] = return uq
pushLListUQ uq@(IOUQueue sz o i j v) (x:xs)
  | i > 0 = do VM.unsafeWrite v (i-1) x
               pushLListUQ uq{uq_i=i-1} xs
  | j < o = do uq' <- recenterUQ uq
               pushLListUQ uq' (x:xs)
  | otherwise = error "pushLListUQ: overflow"

pushLListWithDUQ :: (Unbox a, Unbox b) => IOUQueue (a, b) -> [a] -> b -> IO (IOUQueue (a, b))
pushLListWithDUQ uq [] _ = return uq
pushLListWithDUQ uq@(IOUQueue sz o i j v) (x:xs) d
  | i > 0 = do VM.unsafeWrite v (i-1) (x, d)
               pushLListWithDUQ uq{uq_i=i-1} xs d
  | j < o = do uq' <- recenterUQ uq
               pushLListWithDUQ uq' (x:xs) d
  | otherwise = error "pushLListWithDUQ: overflow"

pushLUQ :: (Unbox a) => IOUQueue a -> a -> IO (IOUQueue a)
pushLUQ uq@(IOUQueue sz o i j v) x
  | i > 0 = do VM.unsafeWrite v (i-1) x
               return $ uq{uq_i=i-1}
  | j < o = do uq' <- recenterUQ uq
               pushLUQ uq' x
  | otherwise = error "pushLUQ: overflow"

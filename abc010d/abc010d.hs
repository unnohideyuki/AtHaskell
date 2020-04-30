{-# LANGUAGE BangPatterns #-}
import           Control.Exception           (assert)
import           Control.Monad
import           Control.Monad.Primitive
import qualified Control.Monad.ST            as ST
import qualified Data.Array.IO               as IO
import qualified Data.Array.ST               as ST
import qualified Data.Array.Unboxed          as A
import           Data.Bits
import qualified Data.ByteString.Char8       as BS
import           Data.Char
import           Data.Foldable
import           Data.List
import           Data.Maybe
import qualified Data.Sequence               as Seq
import qualified Data.Set                    as Set
import qualified Data.Vector                 as VB
import qualified Data.Vector.Mutable         as VBM
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as VM
import           Debug.Trace

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words
getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine
getIntVec n = V.unfoldrN n (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

readInteger = fst . fromJust . BS.readInteger
readIntegerList = map readInteger . BS.words
getInteger = readInteger <$> BS.getLine
getIntegerList = readIntegerList <$> BS.getLine

inf :: Int
inf = 10^18

-- 辺を表す (行先、容量、逆辺（のインデックス）)
type DinicEdge = (Int, Int, Int)

-- グラフの隣接リスト表現
type DinicGraph = VB.Vector (V.Vector DinicEdge)

-- frm から to に向かう容量 cap の辺をグラフに追加する
dinicAddEdge :: DinicGraph -> Int -> Int -> Int -> DinicGraph
dinicAddEdge g frm to cap = g'
  where g_frm = g VB.! frm
        g_to = g VB.! to
        g_frm' = g_frm `V.snoc` (to, cap, V.length g_to)
        g_to'  = g_to  `V.snoc` (frm,  0, V.length g_frm)
        g' = g VB.// [(frm, g_frm'), (to, g_to')]

dinicAddEdgeList g [] = g
dinicAddEdgeList g ((frm, to, cap):es) =
  let g' = dinicAddEdge g frm to cap
  in dinicAddEdgeList g' es

-- s から t への最大流を求める
maxFlow :: DinicGraph -> Int -> Int -> IO Int
maxFlow g s t = do
  g' <- VB.forM g $ \v -> V.thaw v
  let n = VB.length g
  level <- VM.new n -- s からの距離
  VM.set level (0::Int)
  iter <- VM.new n  -- どこまで調べ終わったか
  VM.set iter (0::Int)

  let mfloop :: Int -> IO Int
      mfloop flow = do
        bsf s
        lv_t <- VM.read level t
        if lv_t < 0
          then return flow
          else do VM.set iter 0
                  let loop' flw =
                        do f <- dfs s t (10^18::Int)
                           if f > 0
                             then loop' (flw + f)
                             else return flw
                  flow' <- loop' flow
                  mfloop flow'

      bsf :: Int -> IO ()
      bsf s = do
        VM.set level (-1)
        VM.write level s 0
        let que = Seq.fromList [s]
        bsf' que

      bsf' :: Seq.Seq Int -> IO ()
      bsf' que
        | Seq.null que = return ()
        | otherwise = do
            let v Seq.:< que' = Seq.viewl que
                gv = g' VB.! v
            es <- forM [0..(VM.length gv - 1)] $ \i -> do
              (e_to, e_cap, e_rev) <- VM.read gv i
              lv_to <- VM.read level e_to
              if e_cap > 0 && lv_to < 0
                then do lv_v <- VM.read level v
                        VM.write level e_to (lv_v + 1)
                        return [e_to]
                else return []
            let que'' = que' Seq.>< (Seq.fromList (concat es))
            bsf' que''

      dfs :: Int -> Int -> Int -> IO Int
      dfs v t f
        | v == t    = return f
        | otherwise = do
            let gv = g' VB.! v
            dfs' gv v t f 0

      dfs' gv v t f i
        | i == VM.length gv = return 0
        | otherwise = do
            (e_to, e_cap, e_rev) <- VM.read gv i
            lv_v <- VM.read level v
            lv_to <- VM.read level e_to
            if e_cap > 0 && lv_v < lv_to
              then do d <- dfs e_to t (min f e_cap)
                      if d > 0
                        then do let e_cap' = e_cap - d
                                    gto = g' VB.! e_to
                                VM.write gv i (e_to, e_cap', e_rev)
                                (xto, xcap, xrev) <- VM.read gto e_rev
                                VM.write gto e_rev (xto, xcap+d, xrev)
                                return d
                        else dfs' gv v t f (i+1)
              else dfs' gv v t f (i+1)

  mfloop 0

main = do
  [n, g, e] <- getIntList
  ps <- getIntList
  es <- replicateM e $ do
    [a, b] <- getIntList
    return (a, b, 1)

  let es' = map (\(a, b, c) -> (b, a, c)) es
      elogin = map (\p -> (p, n, 1)) ps
      g = VB.replicate (n+1) V.empty
      g' = dinicAddEdgeList g (es ++ es' ++ elogin)

  f <- maxFlow g' 0 n
  print f

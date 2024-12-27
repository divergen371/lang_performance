{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where

import Control.Parallel.Strategies
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.Random (randomRIO)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

-- 内部ループの計算を行う関数
calcInnerSum :: Int -> Int
calcInnerSum !u = baseSum * quotient + remainderSum
  where
    -- 0からu-1までの余りの合計を計算
    !baseSum = sum [0..u-1]
    
    -- 完全なu個のグループの数と余りを計算
    !quotient = 99_999 `div` u
    !remainder = 99_999 `mod` u
    
    -- 合計を計算
    !remainderSum = sum $ map (`mod` u) [0..remainder]

-- チャンク単位で配列を初期化する関数
initChunk :: Int -> Int -> (Int, Int) -> V.Vector Int
initChunk value size (!start, !end) =
    V.replicate (end - start) value

main :: IO ()
main = do
    args <- getArgs
    case args of
        [arg] -> do
            let u = read arg :: Int
            
            -- 乱数生成
            r <- randomRIO (0, 9_999)
            
            -- 内部ループの計算を1回だけ行う
            let !innerSum = calcInnerSum u
            
            -- 並列処理を使用して配列を初期化
            let !chunks = chunksOf 1_000 10_000
                !value = innerSum + r
                !result = V.concat $
                    parMap rdeepseq (initChunk value 10_000) chunks
            
            -- 結果を出力
            print $ result V.! r
            
        _ -> do
            hPutStrLn stderr "Usage: ./Benchmark <number>"
            exitFailure

-- リストをチャンクに分割する関数
chunksOf :: Int -> Int -> [(Int, Int)]
chunksOf size total =
    let numChunks = (total + size - 1) `div` size
    in [(n * size, min ((n + 1) * size) total) | n <- [0..numChunks-1]]

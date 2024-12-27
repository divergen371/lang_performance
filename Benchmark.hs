module Main where

import System.Environment (getArgs)
import System.Random (randomRIO)
import Data.Array.IO
import Control.Monad (forM_)

main :: IO ()
main = do
    u <- fmap (read . head) getArgs
    r <- randomRIO (0, 9999)
    arr <- newArray (0, 9999) 0 :: IO (IOArray Int Int)
    
    forM_ [0..9999] $ \i -> do
        forM_ [0..99999] $ \j -> do
            val <- readArray arr i
            writeArray arr i (val + j `mod` u)
        val <- readArray arr i
        writeArray arr i (val + r)
    
    result <- readArray arr r
    print result

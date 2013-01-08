{-# LANGUAGE OverloadedStrings #-}

-- | Example program that continously computes the mean of a list of
-- numbers.
module Main where

import Control.Concurrent
import Control.Exception
import qualified System.Remote.Counter as Counter
import qualified System.Remote.Label as Label
import System.Remote.Ekg
import Data.Aeson
import System.Remote.Stats.Histogram

mean :: Fractional a => [a] -> a
mean xs = sum xs / fromIntegral (length xs)

main :: IO ()
main = do
    h <- newUniformHistogram
    update h 3
    update h 5
    mean <- hMean h
    mmin <- hMin h
    mmax <- hMax h
    putStrLn $ show (mmin, mmax, mean)
    --reg <- newRegistry
    --initializeBuiltInStats reg
    --counter <- getCounter "iterations" reg
    --label <- getLabel "args" reg
    --Label.set label "some text string"
    ----let loop n = do
    ----        evaluate $ mean [1..n]
    ----        threadDelay 20
    ----        Counter.inc counter
    ----        loop n
    ----loop 1000
    --stats  <- takeSnapshot reg
    --let lvls = map (\(x, y) -> (x, toJSON y)) $ pullGauges stats
    --let ctrs = map (\(x, y) -> (x, toJSON y)) $ counters stats
    --putStrLn("PullGauges: \n" ++ show lvls)
    --putStrLn("Counters: \n" ++ show ctrs)

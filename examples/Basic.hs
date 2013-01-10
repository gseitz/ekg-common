{-# LANGUAGE OverloadedStrings #-}

-- | Example program that continously computes the mean of a list of
-- numbers.
module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified System.Remote.Counter as Counter
import qualified System.Remote.Label as Label
import System.Remote.Ekg
import Data.Aeson
import System.Remote.Stats.Histogram
import System.Remote.Stats.Meter
import System.Remote.Stats.Time

import System.Posix.Unistd

mean :: Fractional a => [a] -> a
mean xs = sum xs / fromIntegral (length xs)

-- | This function demonstrates Meter
-- The meter is marked with 30 events initially. After that, one thread records
-- 5 events every second, whereas another thread prints the number of marked
-- events and the current 1/5/15 minute rates every 2 seconds.
-- Watch the rates go down from 11 to 5 over time.
runMeter :: IO ()
runMeter = do
    meter <- newMeter Second
    mark meter 30
    forkIO $ forever $ do
        c <- count meter
        m1 <- oneMinuteRate meter
        m5 <- fiveMinuteRate meter
        m15 <- fifteenMinuteRate meter
        putStrLn $ show $ (c, m1, m5, m15)
        sleep 2

    forkIO $ forever $ do
        mark meter 5
        sleep 1

    return ()

runHistogram :: IO ()
runHistogram = do
    h <- newUniformHistogram
    update h 3
    update h 5
    mean <- hMean h
    mmin <- hMin h
    mmax <- hMax h
    putStrLn $ show (mmin, mmax, mean)

main :: IO ()
main = do
    runMeter

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

module System.Remote.Stats.EWMA
	(
      EWMA (..)
    , oneMinuteEWMA
    , fiveMinuteEWMA
    , fifteenMinuteEWMA
    , update
    , tick
    , rate
	) where

import Control.Monad (unless)
import Data.Int (Int64)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef)

import System.Remote.Stats.Atomic (atomicWriteIORef)
import System.Remote.Stats.Time

data EWMA = EWMA
    !(IORef Double)  -- ^ rate
    !Double          -- ^ alpha
    !Integer         -- ^ interval
    !(IORef Int64)   -- ^ uncounted
    !(IORef Bool)    -- ^ initialized

defaultInterval :: Double
defaultInterval = 5.0

defaultIntervalNs :: Integer
defaultIntervalNs = toNanos Second (ceiling defaultInterval)

secondsPerMinute :: Double
secondsPerMinute = 60.0

m1Alpha :: Double
m1Alpha  = mkAlpha defaultInterval 1.0

m5Alpha :: Double
m5Alpha  = mkAlpha defaultInterval 5.0

m15Alpha :: Double
m15Alpha = mkAlpha defaultInterval 15.0

mkAlpha :: Double -> Double -> Double
mkAlpha interval minutes = 1.0 - exp (-interval / secondsPerMinute / minutes)

newEWMA :: Double -> Integer -> IO EWMA
newEWMA alpha ival = do
    rate'       <- newIORef 0.0
    uncounted   <- newIORef 0
    initialized <- newIORef False
    return $ EWMA rate' alpha ival uncounted initialized

oneMinuteEWMA :: IO EWMA
oneMinuteEWMA = newEWMA m1Alpha defaultIntervalNs

fiveMinuteEWMA :: IO EWMA
fiveMinuteEWMA = newEWMA m5Alpha defaultIntervalNs

fifteenMinuteEWMA :: IO EWMA
fifteenMinuteEWMA = newEWMA m15Alpha defaultIntervalNs

update :: EWMA -> Int64 -> IO ()
update (EWMA _ _ _ unc _) n = atomicModifyIORef unc $ \c -> (c+n,())

tick :: EWMA -> IO ()
tick (EWMA rateR alpha interval uncountedR initR) = do
    wasInitialized <- readIORef initR
    uncounted <- atomicModifyIORef uncountedR $ \unc -> (0, unc)
    atomicModifyIORef rateR $ \r -> (calcRate r uncounted wasInitialized,())
    unless wasInitialized $ atomicWriteIORef initR True
  where
    calcRate :: Double -> Int64 -> Bool -> Double
    calcRate r uncounted wasInit =
        let instantRate = fromIntegral uncounted / fromIntegral interval
        in
            if wasInit
            then r + alpha * (instantRate - r)
            else instantRate


rate :: EWMA -> TimeUnit -> IO Double
rate (EWMA rateR _ _ _ _) unit = do
    r <- readIORef rateR
    return $ r * fromIntegral (toNanos unit 1)

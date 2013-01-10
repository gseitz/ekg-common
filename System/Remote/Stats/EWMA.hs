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

import Control.Monad (when)
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

default_interval :: Double
default_interval = 5.0

default_interval_ns :: Integer
default_interval_ns = toNanos Second (ceiling default_interval)

seconds_per_minute :: Double
seconds_per_minute = 60.0

m1_alpha :: Double
m1_alpha  = mkAlpha default_interval 1.0

m5_alpha :: Double
m5_alpha  = mkAlpha default_interval 5.0

m15_alpha :: Double
m15_alpha = mkAlpha default_interval 15.0

mkAlpha :: Double -> Double -> Double
mkAlpha interval minutes = 1.0 - exp (-interval / seconds_per_minute / minutes)

newEWMA :: Double -> Integer -> IO EWMA
newEWMA alpha ival = do
    rate'       <- newIORef 0.0
    uncounted   <- newIORef 0
    initialized <- newIORef False
    return $ EWMA rate' alpha ival uncounted initialized

oneMinuteEWMA :: IO EWMA
oneMinuteEWMA = newEWMA m1_alpha default_interval_ns

fiveMinuteEWMA :: IO EWMA
fiveMinuteEWMA = newEWMA m5_alpha default_interval_ns

fifteenMinuteEWMA :: IO EWMA
fifteenMinuteEWMA = newEWMA m15_alpha default_interval_ns

update :: EWMA -> Int64 -> IO ()
update (EWMA _ _ _ unc _) n = do
    atomicModifyIORef unc (\c -> ((c+n),()))

tick :: EWMA -> IO ()
tick (EWMA rateR alpha interval uncountedR initR) = do
    wasInitialized <- readIORef initR
    uncounted <- atomicModifyIORef uncountedR $ \unc -> (0, unc)
    atomicModifyIORef rateR $ \r -> (calcRate r uncounted wasInitialized,())
    when (not wasInitialized) $ do atomicWriteIORef initR True
  where
    calcRate :: Double -> Int64 -> Bool -> Double
    calcRate r uncounted wasInit =
        let instantRate = (fromIntegral uncounted) / (fromIntegral interval)
        in
            if wasInit
            then r + alpha * (instantRate - r)
            else instantRate


rate :: EWMA -> TimeUnit -> IO Double
rate (EWMA rateR _ _ _ _) unit = do
    r <- readIORef rateR
    return $ r * (fromIntegral $ toNanos unit 1)

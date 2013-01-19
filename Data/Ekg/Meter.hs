module Data.Ekg.Meter
	(
      Meter
    , newMeter
    , inc
    , add
    , count
    , oneMinuteRate
    , fiveMinuteRate
    , fifteenMinuteRate
    , timeUnit
	) where


import Control.Monad (when, forM_)
import Data.Int (Int64)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef)
import Data.Time.Clock.POSIX (getPOSIXTime)

import Data.Atomic
import Data.Ekg.EWMA (EWMA, rate, oneMinuteEWMA, fiveMinuteEWMA,
                                 fifteenMinuteEWMA)
import qualified Data.Ekg.EWMA as E
import Data.TimeUnit (TimeUnit)

-- | A @Meter@ records the number of events that happen and keeps an
-- exponentially-weighted moving average for the 1/5/15 minute rates similar
-- to the UNIX load average.
data Meter = Meter
    !EWMA            -- ^  1min rate
    !EWMA            -- ^  5min rate
    !EWMA            -- ^ 15min rate
    !(IORef Int64)   -- ^ count
    !Double          -- ^ start time
    !(IORef Double)  -- ^ last tick
    !TimeUnit        -- ^ unit for the load averages

-- | Creates a new meter with the given @TimeUnit@ for the load averages.
newMeter :: TimeUnit -> IO Meter
newMeter unit = do
    c <- newIORef 0
    time <- getTimeMillis
    lastTick <- newIORef time
    m1 <- oneMinuteEWMA
    m5 <- fiveMinuteEWMA
    m15 <- fifteenMinuteEWMA
    return $ Meter m1 m5 m15 c time lastTick unit

-- | Records the occurrence of a single event.
inc :: Meter -> IO ()
inc m = add m 1

-- | Records the occurrence of a number of events.
add :: Meter
    -> Int64  -- ^ The number of events that occurred
    -> IO ()
add m@(Meter m1 m5 m15 countR _ _ _) n = do
    tickIfNecessary m
    atomicModifyIORef countR $ \c -> (c+n, ())
    E.update m1 n
    E.update m5 n
    E.update m15 n

-- | Gets the number of recorded events so far.
count :: Meter -> IO Int64
count (Meter _ _ _ countR _ _ _) = readIORef countR

-- | Gets the 1 minute rate of the provided @Meter@.
oneMinuteRate :: Meter -> IO Double
oneMinuteRate (Meter m1 _ _ _ _ _ unit) = rate m1 unit

-- | Gets the 5 minute rate of the provided @Meter@.
fiveMinuteRate :: Meter -> IO Double
fiveMinuteRate (Meter _ m5 _ _ _ _ unit) = rate m5 unit

-- | Gets the 15 minute rate of the provided @Meter@.
fifteenMinuteRate :: Meter -> IO Double
fifteenMinuteRate (Meter _ _ m15 _ _ _ unit) = rate m15 unit

-- | Gets the time unit used for the load averages.
timeUnit :: Meter -> TimeUnit
timeUnit (Meter _ _ _ _ _ _ unit) = unit

getTimeMillis :: IO Double
getTimeMillis = (realToFrac . (* 1000)) `fmap` getPOSIXTime

tickInterval :: Int
tickInterval = 5000

tick :: Meter -> IO ()
tick (Meter m1 m5 m15 _ _ _ _) = do
    E.tick m1
    E.tick m5
    E.tick m15

tickIfNecessary :: Meter -> IO ()
tickIfNecessary m@(Meter _ _ _ _ _ lastR _) = do
    oldTick <- readIORef lastR
    newTick <- getTimeMillis
    let age = floor $ newTick - oldTick
    when (age > tickInterval) $ do
        wasSet <- compareAndSet lastR oldTick newTick
        when wasSet $
            let requiredTicks = age `div` tickInterval
            in forM_ [1..requiredTicks] $ \_ -> tick m

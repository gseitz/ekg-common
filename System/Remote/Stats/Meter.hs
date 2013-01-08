module System.Remote.Stats.Meter
	(
      Meter (..)
    , mark
    , mark1
    , count
	) where


import System.Remote.Stats.EWMA (EWMA)
import qualified System.Remote.Stats.EWMA as E
import Data.Int (Int64)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Time.Units
import Data.IORef
import System.Remote.Stats.Atomic
import Control.Monad (when, forM_)

data Meter = Meter EWMA -- ^  1min rate
                   EWMA -- ^  5min rate
                   EWMA -- ^ 15min rate
                  (IORef Int64) -- ^ count
                   Double -- ^ start time
                  (IORef Double) -- ^ last tick


tick_interval :: Int
tick_interval = 5000



tick :: Meter -> IO ()
tick (Meter m1 m5 m15 _ _ _) = do
    E.tick m1
    E.tick m5
    E.tick m15

mark1 :: Meter -> IO ()
mark1 m = mark m 1

mark :: Meter -> Int64 -> IO ()
mark m@(Meter m1 m5 m15 countR _ _) n = do
    tickIfNecessary m
    atomicModifyIORef' countR (\c -> (c+n, ()))
    E.update m1 n
    E.update m5 n
    E.update m15 n


tickIfNecessary :: Meter -> IO ()
tickIfNecessary m@(Meter _ _ _ _ _ lastR) = do
    oldTick <- readIORef lastR
    newTick <- getTimeMillis
    age <- return $ floor $ newTick - oldTick
    when (age > tick_interval) $ do
        wasSet <- compareAndSet lastR oldTick newTick
        requiredTicks <- return $ div age tick_interval
        forM_ [1..requiredTicks] (\_ -> tick m)


count :: Meter -> IO Int64
count (Meter _ _ _ countR _ _) = do
    readIORef countR




getTimeMillis :: IO Double
getTimeMillis = (realToFrac . (* 1000)) `fmap` getPOSIXTime



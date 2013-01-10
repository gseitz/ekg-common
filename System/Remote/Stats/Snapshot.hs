module System.Remote.Stats.Snapshot
	(
      Snapshot
    , newSnapshot
    , size
    , median
    , p75thPercentile
    , p95thPercentile
    , p98thPercentile
    , p99thPercentile
    , p999thPercentile
    , values
	) where

import Data.List (sort)
import qualified Data.Vector.Unboxed as V


newtype Snapshot = Snapshot (V.Vector Double)

-- | Creates a new @Snapshot@ out of a list of @Double@ values.
newSnapshot :: [Double] -> Snapshot
newSnapshot = Snapshot . V.fromList . sort

-- | Gets the number of values in the provided @Snapshot@.
size :: Snapshot -> Int
size (Snapshot xs) = V.length xs

-- | Gets the median value of the provided @Snapshot@.
median :: Snapshot -> Double
median = getValue 0.5

-- | Gets the 75th percentile of the provided @Snapshot@.
p75thPercentile :: Snapshot -> Double
p75thPercentile = getValue 0.75

-- | Gets the 95th percentile of the provided @Snapshot@.
p95thPercentile :: Snapshot -> Double
p95thPercentile = getValue 0.95

-- | Gets the 98th percentile of the provided @Snapshot@.
p98thPercentile :: Snapshot -> Double
p98thPercentile = getValue 0.98

-- | Gets the 99th percentile of the provided @Snapshot@.
p99thPercentile :: Snapshot -> Double
p99thPercentile = getValue 0.99

-- | Gets the 99.9th percentile of the provided @Snapshot@.
p999thPercentile :: Snapshot -> Double
p999thPercentile = getValue 0.999

-- | Gets all values of the provided @Snapshot@.
values :: Snapshot -> [Double]
values (Snapshot ds) = V.toList ds


getValue :: Double -> Snapshot -> Double
getValue q (Snapshot xs)
    | len == 0                  = 0.0
    | pos < 1                   = V.head xs
    | pos >= (fromIntegral len) = V.last xs
    | otherwise                 = lower + (pos - pos') * (upper - lower)
  where
    pos   = q * (fromIntegral (len + 1))
    pos'  = (fromIntegral $ floor pos)
    len   = V.length xs :: Int
    lower = xs V.! (floor pos - 1)
    upper = xs V.! (floor pos)

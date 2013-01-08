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

newSnapshot :: [Double] -> Snapshot
newSnapshot = Snapshot . V.fromList . sort


getValue :: Double -> Snapshot -> Double
getValue q (Snapshot xs)
    | len == 0                  = 0.0
    | pos < 1                   = V.head xs
    | pos >= (fromIntegral len) = V.last xs
    | otherwise                 = lower + (pos - (fromIntegral $ floor pos)) * (upper - lower)
  where pos = q * (fromIntegral (len + 1))
        len = V.length xs :: Int
        lower = xs V.! (floor pos - 1)
        upper = xs V.! (floor pos)

size :: Snapshot -> Int
size (Snapshot xs) = V.length xs

median :: Snapshot -> Double
median = getValue 0.5

p75thPercentile :: Snapshot -> Double
p75thPercentile = getValue 0.75

p95thPercentile :: Snapshot -> Double
p95thPercentile = getValue 0.95

p98thPercentile :: Snapshot -> Double
p98thPercentile = getValue 0.98

p99thPercentile :: Snapshot -> Double
p99thPercentile = getValue 0.99

p999thPercentile :: Snapshot -> Double
p999thPercentile = getValue 0.999

values :: Snapshot -> [Double]
values (Snapshot ds) = V.toList ds
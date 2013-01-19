{-# LANGUAGE OverloadedStrings #-}
module Data.Ekg.Histogram.HistogramSnapshot
	(
      HistogramSnapshot (HistoSnap)
	) where

import qualified Data.Aeson as A
import Data.Aeson.Types ((.=))
import Data.Int (Int64)

import Data.Ekg.Histogram.SampleSnapshot


data HistogramSnapshot = HistoSnap !SampleSnapshot  -- ^ Sample snapshot
                                   !Int64           -- ^ Count
                                   !Int64           -- ^ Sum
                                   !Int64           -- ^ Min
                                   !Int64           -- ^ Max
                                   !Double          -- ^ Mean
                                   !Double          -- ^ StdDev


instance A.ToJSON HistogramSnapshot where
    toJSON (HistoSnap snap count hsum hmin hmax mean stdev) = A.object
        [ "count" .= count
        , "sum"  .= hsum
        , "min" .= hmin
        , "max" .= hmax
        , "mean" .= mean
        , "stdev" .= stdev
        , "median" .= (median snap)
        , "p75" .= (p75thPercentile snap)
        , "p95" .= (p95thPercentile snap)
        , "p98" .= (p98thPercentile snap)
        , "p99" .= (p99thPercentile snap)
        , "p999" .= (p999thPercentile snap)
        ]

{-# LANGUAGE OverloadedStrings #-}
module Data.Ekg.Meter.MeterSnapshot
	(
      MeterSnapshot
    , meterSnapshot
	) where

import Data.Aeson.Types ((.=), ToJSON (..), object)
import Data.Int (Int64)

import Data.Ekg.Meter
import Data.TimeUnit (TimeUnit)

data MeterSnapshot = MeterSnapshot !Double    -- ^ 1 minute rate
                                   !Double    -- ^ 5 minute rate
                                   !Double    -- ^ 15 minute rate
                                   !Int64     -- ^ count
                                   !TimeUnit  -- ^ the time unit of the rates

instance ToJSON MeterSnapshot where
    toJSON (MeterSnapshot m1 m5 m15 c unit) = object
        [ "one_minute_rate"     .= m1
        , "five_minute_rate"    .= m5
        , "fifteen_minute_rate" .= m15
        , "count"               .= c
        , "unit"                .= unit
        ]

meterSnapshot :: Meter -> IO MeterSnapshot
meterSnapshot m = do
    m1 <- oneMinuteRate m
    m5 <- fiveMinuteRate m
    m15 <- fifteenMinuteRate m
    c <- count m
    return $ MeterSnapshot m1 m5 m15 c (timeUnit m)


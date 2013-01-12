{-# OPTIONS_HADDOCK not-home #-}
module Data.Ekg.PullGauge.Internal
    (
      PullGauge(..)
    , new
    , read
    ) where

import Prelude hiding (read)

-- | A mutable, integer-valued gauge.
newtype PullGauge = C { unc :: IO Int }

-- | Create a new, zero initialized, gauge.
new :: IO Int -> PullGauge
new = C

read :: PullGauge -> IO Int
read pullGauge = unc pullGauge

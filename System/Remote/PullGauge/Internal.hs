{-# OPTIONS_HADDOCK not-home #-}
module System.Remote.PullGauge.Internal
    (
      PullGauge(..)
    , new
    , read
    ) where

import Prelude hiding (read)

-- | A mutable, integer-valued gauge.
newtype PullGauge = P { unP :: IO Int }

-- | Create a new, zero initialized, gauge.
new :: IO Int -> PullGauge
new = P

read :: PullGauge -> IO Int
read pullGauge = unP pullGauge

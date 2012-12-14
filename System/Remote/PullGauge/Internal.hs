{-# OPTIONS_HADDOCK not-home #-}
module System.Remote.PullGauge.Internal
    (
      PullGauge(..)
    , new
    , read
    ) where

import Data.IORef (IORef, newIORef, readIORef)
import Prelude hiding (read)

-- | A mutable, integer-valued gauge.
newtype PullGauge = C { unC :: IORef (IO Int) }

-- | Create a new, zero initialized, gauge.
new :: IO (PullGauge)
new = C `fmap` newIORef (return 0)

read :: PullGauge -> IO Int
read pullGauge = do 
                ref <- readIORef . unC $ pullGauge
                value <- ref
                return value

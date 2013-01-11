{-# OPTIONS_HADDOCK not-home #-}
module Data.Ekg.Counter.Internal
    (
      Counter(..)
    , new
    , read
    ) where

import Data.IORef (IORef, newIORef, readIORef)
import Prelude hiding (read)

-- | A mutable, integer-valued counter.
newtype Counter = C { unC :: IORef Int }

-- | Create a new, zero initialized, counter.
new :: IO Counter
new = C `fmap` newIORef 0

read :: Counter -> IO Int
read = readIORef . unC

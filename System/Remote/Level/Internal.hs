{-# OPTIONS_HADDOCK not-home #-}
module System.Remote.Level.Internal
    (
      Level(..)
    , new
    , read
    ) where

import Data.IORef (IORef, newIORef, readIORef)
import Prelude hiding (read)

-- | A mutable, integer-valued gauge.
newtype Level = C { unC :: IORef (IO Int) }

-- | Create a new, zero initialized, gauge.
new :: IO (Level)
new = C `fmap` newIORef (return 0)

read :: Level -> IO Int
read level = do 
                ref <- readIORef . unC $ level
                value <- ref
                return value

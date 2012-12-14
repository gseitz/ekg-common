{-# LANGUAGE BangPatterns #-}
-- | This module defines a type for self-updating, integer-valued pullGauges.
-- PullGauges are variable values and update themselves when their current state is queried. 
-- All operations on pullGauges are thread-safe.
module System.Remote.PullGauge
    (
      PullGauge
    , set
    ) where

import Data.IORef (atomicModifyIORef)
import Prelude hiding (subtract)

import System.Remote.PullGauge.Internal

-- | Set the pullGauge to the given value.
set :: PullGauge -> IO Int -> IO ()
set (C ref) f = atomicModifyIORef ref $ \ _ -> (f, ())
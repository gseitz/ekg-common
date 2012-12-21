{-# LANGUAGE BangPatterns #-}
-- | This module defines a type for self-updating, integer-valued pullGauges.
-- PullGauges are variable values and update themselves when their current state is queried. 
-- All operations on pullGauges are thread-safe.
module System.Remote.PullGauge
    (
      PullGauge
    ) where

import System.Remote.PullGauge.Internal

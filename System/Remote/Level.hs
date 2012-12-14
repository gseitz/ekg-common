{-# LANGUAGE BangPatterns #-}
-- | This module defines a type for self-updating, integer-valued levels.
-- Levels are variable values and update themselves when their current state is queried. 
-- All operations on levels are thread-safe.
module System.Remote.Level
    (
      Level
    , set
    ) where

import Data.IORef (atomicModifyIORef)
import Prelude hiding (subtract)

import System.Remote.Level.Internal

-- | Set the level to the given value.
set :: Level -> IO Int -> IO ()
set (C ref) f = atomicModifyIORef ref $ \ _ -> (f, ())
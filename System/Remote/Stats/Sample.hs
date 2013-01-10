module System.Remote.Stats.Sample
    (
      Sample(..)
    ) where


import Data.Int (Int64)

import System.Remote.Stats.Snapshot (Snapshot)

class Sample s where
    -- | Gets the current number of values used in the @Sample@.
    size     :: s       -- ^ Sample
             -> IO Int  -- ^ Number of values used in the @Sample@

    -- | Adds a value to the @Sample@.
    update   :: s      -- ^ Sample
             -> Int64  -- ^ Value to put into the @Sample@
             -> IO ()

    -- | Gets a snapshot of the values in the @Sample@.
    snapshot :: s            -- ^ Sample
             -> IO Snapshot  -- ^ Snapshot of the current values in the @Sample@

    -- | Clears all data in the @Sample@.
    clear    :: s      -- ^ Sample
             -> IO ()






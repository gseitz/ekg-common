module System.Remote.Stats.Sample
    (
      Sample(..)
    ) where


import Data.Int

import System.Remote.Stats.Snapshot (Snapshot)

class Sample s where
    size     :: s -> IO Int
    update   :: s -> Int64 -> IO ()
    snapshot :: s -> IO Snapshot
    clear    :: s -> IO ()






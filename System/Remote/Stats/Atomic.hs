module System.Remote.Stats.Atomic
	(
      compareAndSet
	) where

import Data.IORef

compareAndSet :: Eq a => IORef a -> a -> a -> IO Bool
compareAndSet ref old new = do
    atomicModifyIORef' ref (\current -> if (current == old) then (new, True) else (current, False))
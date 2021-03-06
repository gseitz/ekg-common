module Data.Atomic
	(
      atomicWriteIORef
    , compareAndSet
	) where

import Data.IORef (IORef, atomicModifyIORef)

atomicWriteIORef :: IORef a -> a -> IO ()
atomicWriteIORef ref a = atomicModifyIORef ref $ const (a, ())

compareAndSet :: Eq a => IORef a -> a -> a -> IO Bool
compareAndSet ref old new =
    atomicModifyIORef ref $ \current -> if current == old
        then (new, True)
        else (current, False)

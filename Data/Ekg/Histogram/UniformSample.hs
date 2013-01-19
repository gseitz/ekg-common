module Data.Ekg.Histogram.UniformSample
	(
      UniformSample
    , newUniformSample
	) where


import Control.Monad (forM_, when)
import Data.Array.MArray
import Data.Array.IO (IOArray)
import Data.Int
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef)
import System.Random

import Data.Atomic (atomicWriteIORef)
import qualified Data.Ekg.Histogram.Sample as S
import Data.Ekg.Histogram.SampleSnapshot (newSnapshot)


data UniformSample = UniformSample
    !(IOArray Int Int64)  -- ^ values
    !(IORef Int64)        -- ^ count

instance S.Sample UniformSample where
    size (UniformSample values count) = do
        c  <- readIORef count
        (_,hi) <- getBounds values
        return $ min (fromIntegral c) hi

    update (UniformSample values count) value = do
        c      <- atomicModifyIORef count $ \n -> (n+1, n+1)
        (_,hi) <- getBounds values
        if c-1 <= fromIntegral hi
            then writeArray values (fromIntegral (c-1)) value
            else do
                r <- nextInt $ fromIntegral c
                when (r <= hi) $ writeArray values r value

    snapshot sample @ (UniformSample values _) = do
        c  <- S.size sample
        ds <- getElems values
        return $ newSnapshot $ take c $ map fromIntegral ds

    clear (UniformSample values count) = do
        atomicWriteIORef count 0
        (lo,hi) <- getBounds values
        forM_ [lo..hi] $ \i -> writeArray values i 0

newUniformSample :: Int -> IO UniformSample
newUniformSample size = do
    vals  <- newArray (0, size-1) 0
    count <- newIORef 0
    return $ UniformSample vals count

nextInt :: Int -> IO Int
nextInt n = randomRIO (0, n)
module System.Remote.Stats.Sample.UniformSample
	(
      UniformSample
    , newUniformSample
	) where


import Control.Monad (forM_, when)
import Data.Array.MArray
import Data.Array.IO (IOArray)
import Data.Int
import Data.IORef (IORef, newIORef, readIORef, atomicWriteIORef, atomicModifyIORef')
import System.Random

import qualified System.Remote.Stats.Sample as S
import System.Remote.Stats.Snapshot (newSnapshot)


data UniformSample = UniformSample 
    !(IOArray Int Int64)  -- ^ values
    !(IORef Int64)        -- ^ count

instance S.Sample UniformSample where 
    size (UniformSample values count) = do
        c  <- readIORef count
        (_,hi) <- getBounds values
        return $ min (fromIntegral c) hi

    update (UniformSample values count) value = do
        c      <- atomicModifyIORef' count $ \n -> (n+1, n+1)    
        (_,hi) <- getBounds values
        when (c-1 <= fromIntegral hi) $
            writeArray values (fromIntegral (c-1)) value
        r <- nextInt $ fromIntegral c
        when (c   >= fromIntegral hi && r <= hi) $ writeArray values r value

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
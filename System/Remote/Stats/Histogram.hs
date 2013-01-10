{-# LANGUAGE ExistentialQuantification #-}
module System.Remote.Stats.Histogram
    (
      Histogram
    , newUniformHistogram
    , clear
    , update
    , hMin
    , hMax
    , hMean
    , hSum
    , hVariance
    , hCount
    , hSnapshot
    , hStdDev
    ) where

import Control.Monad (when)
import Data.Int (Int64)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef)

import System.Remote.Stats.Atomic (atomicWriteIORef)
import System.Remote.Stats.Sample.UniformSample
import System.Remote.Stats.Snapshot (Snapshot)
import qualified System.Remote.Stats.Sample as S


data Histogram = forall a. S.Sample a => Histogram
    a                         -- ^ sample
    (IORef Int64)             -- ^ min
    (IORef Int64)             -- ^ max
    (IORef Int64)             -- ^ sum
    (IORef Int64)             -- ^ count
    (IORef (Double, Double))  -- ^ variance


default_sample_size :: Int
default_sample_size = 1028

newUniformHistogram :: IO Histogram
newUniformHistogram = do
    sample <- newUniformSample default_sample_size
    minR   <- newIORef (maxBound)
    maxR   <- newIORef (minBound)
    sumR   <- newIORef 0
    varR   <- newIORef (-1, 0)
    countR <- newIORef 0
    return $ Histogram sample minR maxR sumR countR varR

clear :: Histogram -> IO ()
clear (Histogram sample minR maxR sumR countR varR) = do
    S.clear sample
    atomicWriteIORef minR 0
    atomicWriteIORef maxR 0
    atomicWriteIORef sumR 0
    atomicWriteIORef countR 0
    atomicWriteIORef varR (-1, 0)

update :: Histogram -> Int64 -> IO ()
update h@(Histogram sample minR maxR sumR countR _) value = do
    S.update sample value
    setMin minR value
    setMax maxR value
    atomicModifyIORef sumR $ \s -> (s+value, ())
    atomicModifyIORef countR $ \x -> (x+1, ())
    updateVariance h value


updateVariance :: Histogram -> Int64 -> IO ()
updateVariance h@(Histogram _ _ _ _ _ varR) value = do
    old@(oldM, oldS) <- readIORef varR
    new <- if oldM == -1
        then return (valued, 0.0)
        else let newM x = oldM + ((valued-oldM) / x)
                 newS x = oldS + (valued-oldM) * (valued - (newM x))
             in do
                 c <- hCount h
                 return $ (newM $ fromIntegral c, newS $ fromIntegral c)
    succeeded <- atomicModifyIORef varR  $ \t ->
        if t == old
            then (new, True)
            else (t, False)
    when (not succeeded) $ updateVariance h value
  where
    valued = fromIntegral value



hCount :: Histogram -> IO Int64
hCount (Histogram _ _ _ _ countR _) = do 
    readIORef countR

hSum :: Histogram -> IO Int64
hSum (Histogram _ _ _ sumR _ _) = do 
    readIORef sumR

hMean :: Histogram -> IO Double
hMean h = do
    s <- hSum h
    c <- hCount h
    return $ (fromIntegral s) / (fromIntegral c)

hMin :: Histogram -> IO Double
hMin h@(Histogram _ minR _ _ _ _) = do
    c <- hCount h
    m <- readIORef minR
    return $ if c > 0 then fromIntegral m else 0.0

hMax :: Histogram -> IO Double
hMax h@(Histogram _ _ maxR _ _ _) = do
    c <- hCount h
    m <- readIORef maxR
    return $ if c > 0 then fromIntegral m else 0.0

hVariance :: Histogram -> IO Double
hVariance h@(Histogram _ _ _ _ _ varR) = do
    c <- hCount h
    (_, v) <- readIORef varR
    return $ if c <= 1 then 0.0 else v / (fromIntegral c-1)

hStdDev :: Histogram -> IO Double
hStdDev h = do
    c <- hCount h
    v <- hVariance h
    return $ if c > 0 then sqrt v else 0.0

hSnapshot :: Histogram -> IO Snapshot
hSnapshot (Histogram sample _ _ _ _ _) = do
    S.snapshot sample

setMax :: IORef Int64 -> Int64 -> IO ()
setMax ref val = set ref (<) val

setMin :: IORef Int64 -> Int64 -> IO ()
setMin ref val = set ref (>) val

set :: IORef Int64 -> (Int64 -> Int64 -> Bool) -> Int64 -> IO ()
set ref p val = do
    s <- readIORef ref
    when (p s val) $ atomicModifyIORef ref $ \x -> (newX x, ())
  where
    newX x = if p x val then val else x




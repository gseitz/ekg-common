{-# LANGUAGE ExistentialQuantification #-}
module Data.Ekg.Histogram
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

import Control.Monad (when, unless)
import Data.Int (Int64)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef)

import Data.Atomic (atomicWriteIORef)
import Data.Ekg.Histogram.HistogramSnapshot
import qualified Data.Ekg.Histogram.Sample as S
import Data.Ekg.Histogram.SampleSnapshot (SampleSnapshot)
import Data.Ekg.Histogram.UniformSample


data Histogram = forall a. S.Sample a => Histogram
    !a                         -- ^ sample
    !(IORef Int64)             -- ^ min
    !(IORef Int64)             -- ^ max
    !(IORef Int64)             -- ^ sum
    !(IORef Int64)             -- ^ count
    !(IORef (Double, Double))  -- ^ variance


defaultSampleSize :: Int
defaultSampleSize = 1028

newUniformHistogram :: IO Histogram
newUniformHistogram = newHistogram =<< newUniformSample defaultSampleSize

newHistogram :: S.Sample a => a -> IO Histogram
newHistogram sample = do
    minR   <- newIORef maxBound
    maxR   <- newIORef minBound
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
                 newS x = oldS + (valued-oldM) * (valued - newM x)
             in do
                 c <- hCount h
                 return (newM $ fromIntegral c, newS $ fromIntegral c)
    succeeded <- atomicModifyIORef varR  $ \t ->
        if t == old
            then (new, True)
            else (t, False)
    unless succeeded $ updateVariance h value
  where
    valued = fromIntegral value



hCount :: Histogram -> IO Int64
hCount (Histogram _ _ _ _ countR _) = readIORef countR

hSum :: Histogram -> IO Int64
hSum (Histogram _ _ _ sumR _ _) = readIORef sumR

hMean :: Histogram -> IO Double
hMean h = do
    s <- hSum h
    c <- hCount h
    return $ fromIntegral s / fromIntegral c

hMin :: Histogram -> IO Double
hMin h@(Histogram _ minR _ _ _ _) = do
    c <- hCount h
    if c < 1
        then return 0.0
        else fromIntegral `fmap` readIORef minR

hMax :: Histogram -> IO Double
hMax h@(Histogram _ _ maxR _ _ _) = do
    c <- hCount h
    if c < 1
        then return 0.0
        else fromIntegral `fmap` readIORef maxR

hVariance :: Histogram -> IO Double
hVariance h@(Histogram _ _ _ _ _ varR) = do
    c <- hCount h
    if c <= 1
        then return 0.0
        else do
            (_, v) <- readIORef varR
            return $ v / (fromIntegral c-1)

hStdDev :: Histogram -> IO Double
hStdDev h = do
    c <- hCount h
    if c < 1
        then return 0.0
        else sqrt `fmap` hVariance h

hSnapshot :: Histogram -> IO SampleSnapshot
hSnapshot (Histogram sample _ _ _ _ _) = S.snapshot sample

setMax :: IORef Int64 -> Int64 -> IO ()
setMax ref = set ref (<)

setMin :: IORef Int64 -> Int64 -> IO ()
setMin ref = set ref (>)

set :: IORef Int64 -> (Int64 -> Int64 -> Bool) -> Int64 -> IO ()
set ref p val = do
    s <- readIORef ref
    when (p s val) $ atomicModifyIORef ref $ \x -> (newX x, ())
  where
    newX x = if p x val then val else x




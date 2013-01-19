{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE FunctionalDependencies, OverloadedStrings #-}
module Data.Ekg.Registry.Internal
    (
      Registry(..)
    , Ref(..)
    , getRef
    , newRegistry
    ) where

import qualified Data.Text as T
import qualified Data.HashMap.Strict as M
import Data.IORef (IORef, atomicModifyIORef, newIORef)

import qualified Data.Ekg.Counter.Internal as Counter
import qualified Data.Ekg.Gauge.Internal as Gauge
import Data.Ekg.Histogram (Histogram, captureSnapshot)
import Data.Ekg.Histogram.HistogramSnapshot (HistogramSnapshot)
import qualified Data.Ekg.Label.Internal as Label
import Data.Ekg.Meter  (Meter)
import Data.Ekg.Meter.MeterSnapshot
import qualified Data.Ekg.PullGauge.Internal as PullGauge
import System.Remote.Gauge
import System.Remote.Counter
import System.Remote.Label
import System.Remote.PullGauge


-- Map of user-defined counters.
type Counters = M.HashMap T.Text Counter

-- Map of user-defined gauges.
type Gauges = M.HashMap T.Text Gauge

-- Map of user-defined labels.
type Labels = M.HashMap T.Text Label

-- Map of user-defined pullGauges.
type PullGauges = M.HashMap T.Text PullGauge

-- Map of user-defined histograms.
type Histograms = M.HashMap T.Text Histogram

-- Map of user-defined meters.
type Meters = M.HashMap T.Text Meter

-- | A handle that can be used as a centralized point of access and/or grouping mechanism for metrics.
data Registry = Registry {
      userCounters :: !(IORef Counters)
    , userGauges :: !(IORef Gauges)
    , userLabels :: !(IORef Labels)
    , userPullGauges :: !(IORef PullGauges)
    , userHistograms :: !(IORef Histograms)
    , userMeters :: !(IORef Meters)
    }

-- Creates an empty Registry
newRegistry :: IO Registry
newRegistry = do
    counters <- newIORef M.empty
    gauges <- newIORef M.empty
    labels <- newIORef M.empty
    pullGauges <- newIORef M.empty
    histograms <- newIORef M.empty
    meters <- newIORef M.empty
    return $ Registry counters gauges labels pullGauges histograms meters


class Ref r t | r -> t where
    read :: r -> IO t

instance Ref Counter Int where
    read = Counter.read

instance Ref Gauge Int where
    read = Gauge.read

instance Ref Label T.Text where
    read = Label.read

instance Ref PullGauge Int where
    read = PullGauge.read

instance Ref Histogram HistogramSnapshot where
    read = captureSnapshot

instance Ref Meter MeterSnapshot where
    read = meterSnapshot


-- | Lookup a 'Ref' by name in the given map.  If no 'Ref' exists
-- under the given name, create a new one, insert it into the map and
-- return it.
getRef :: T.Text                      -- ^ 'Ref' name
       -> IO r                        -- ^ the 'Ref', in case it doesn't exist yet
       -> IORef (M.HashMap T.Text r)  -- ^ Map that will store the 'Ref'
       -> IO r
getRef name new mapRef = do
    empty <- new
    atomicModifyIORef mapRef $ \ m ->
        case M.lookup name m of
            Nothing  -> let m' = M.insert name empty m
                        in (m', empty)
            Just ref -> (m, ref)
{-# INLINABLE getRef #-}

-- | This module defines a type for mutable, integer-valued counters.
-- Counters are non-negative, monotonically increasing values and can
-- be used to track e.g. the number of requests served since program
-- start.  All operations on counters are thread-safe.
module Data.Ekg.Registry
    (
      Registry
    , getCounter
    , getGauge
    , getHistogram
    , getLabel
    , getMeter
    , newPullGauge
    , getPullGauge
    , newRegistry

    , hasCounter
    , hasGauge
    , hasHistogram
    , hasLabel
    , hasMeter
    , hasPullGauge

    , Counter
    , Gauge
    , Label
    , PullGauge
    , Ref
    ) where

import Data.Functor
import qualified Data.HashMap.Strict as M
import Data.IORef (IORef, readIORef)
import qualified Data.Text as T

import Data.TimeUnit
import qualified Data.Ekg.Counter.Internal as C
import qualified Data.Ekg.Gauge.Internal as G
import qualified Data.Ekg.Label.Internal as L
import Data.Ekg.Meter (Meter)
import qualified Data.Ekg.Meter as Meter
import Data.Ekg.Histogram (Histogram)
import qualified Data.Ekg.Histogram as Histogram
import qualified Data.Ekg.PullGauge.Internal as P
import Data.Ekg.Registry.Internal
import System.Remote.Gauge
import System.Remote.Counter
import System.Remote.Label
import System.Remote.PullGauge

-- | Return the counter associated with the given name and registry.
-- Multiple calls to 'getCounter' with the same arguments will return
-- the same counter.  The first time 'getCounter' is called for a
-- given name and registry, a new, zero-initialized counter will be
-- returned.
getCounter :: T.Text  -- ^ Counter name
           -> Registry  -- ^ Registry that contains the counter
           -> IO Counter
getCounter name registry = getRef name C.new (userCounters registry)

-- | Return the gauge associated with the given name and registry.
-- Multiple calls to 'getGauge' with the same arguments will return
-- the same gauge.  The first time 'getGauge' is called for a given
-- name and registry, a new, zero-initialized gauge will be returned.
getGauge :: T.Text  -- ^ Gauge name
         -> Registry  -- ^ Registry that contains the gauge
         -> IO Gauge
getGauge name registry = getRef name G.new (userGauges registry)

-- | Return the label associated with the given name and registry.
-- Multiple calls to 'getLabel' with the same arguments will return
-- the same label.  The first time 'getLabel' is called for a given
-- name and registry, a new, empty label will be returned.
getLabel :: T.Text  -- ^ Label name
         -> Registry  -- ^ Registry that contains the label
         -> IO Label
getLabel name registry = getRef name L.new (userLabels registry)

-- | Return the pullGauge associated with the given name and registry.
-- Multiple calls to 'getPullGauge' with the same arguments will return
-- the same pullGauge.  The first time 'getPullGauge' is called for a given
-- name and registry, a new, empty pullGauge will be returned.
newPullGauge :: T.Text -- ^ PullGauge name
		     -> Registry -- ^ Registry for which to create the pullGauge
             -> IO Int
		     -> IO PullGauge
newPullGauge name registry value =
    getRef name (return $ P.new value) (userPullGauges registry)

getPullGauge :: T.Text -- ^ PullGauge name
             -> Registry -- ^ The registry that contains the pullGauge
             -> IO (Maybe PullGauge)
getPullGauge name registry =
    M.lookup name <$> readIORef (userPullGauges registry)

-- | Return the histogram associated with the given name and registry.
-- Multiple calls to 'getHistogram' with the same arguments will return the
-- same histogram. The first time 'getHistogram' is called for a given
-- name and registry, a new Histogram will be returned.
getHistogram :: T.Text -- ^ Histogram name
             -> Registry -- ^ The registry that contains the histogram
             -> IO Histogram
getHistogram name registry =
    getRef name Histogram.newUniformHistogram (userHistograms registry)

-- | Return the meter associated with the given name and registry.
-- Multiple calls to 'getMeter' with the same arguments will return the
-- same meter. The first time 'getmeter' is called for a given
-- name and registry, a new Meter will be returned.
getMeter :: T.Text  -- ^ Histogram name
         -> Registry  -- ^ The registry that contains the histogram
         -> TimeUnit  -- ^ The @TimeUnit@ used for the rates.
         -> IO Meter
getMeter name registry unit =
    getRef name (Meter.newMeter unit) (userMeters registry)

hasCounter :: T.Text -- ^ Counter name
           -> Registry
           -> IO Bool
hasCounter name registry = hasRef name $ userCounters registry


hasGauge :: T.Text -- ^ Gauge name
         -> Registry
         -> IO Bool
hasGauge name registry = hasRef name $ userGauges registry

hasLabel :: T.Text -- ^ Label name
         -> Registry
         -> IO Bool
hasLabel name registry = hasRef name $ userGauges registry

hasPullGauge :: T.Text -- ^ PullGauge name
             -> Registry
             -> IO Bool
hasPullGauge name registry = hasRef name $ userPullGauges registry

hasHistogram :: T.Text  -- ^ Histogram name
             -> Registry
             -> IO Bool
hasHistogram name registry = hasRef name $ userHistograms registry

hasMeter :: T.Text  -- ^ Meter name
         -> Registry
         -> IO Bool
hasMeter name registry = hasRef name $ userMeters registry

hasRef :: T.Text
       -> IORef (M.HashMap T.Text r)
       -> IO Bool
hasRef name ioref = do
                cs <- readIORef ioref
                return $ M.member name cs
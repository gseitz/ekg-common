{-# LANGUAGE BangPatterns #-}
-- | This module defines a type for mutable, integer-valued counters.
-- Counters are non-negative, monotonically increasing values and can
-- be used to track e.g. the number of requests served since program
-- start.  All operations on counters are thread-safe.
module System.Remote.Registry
    (
      Registry
    , getCounter
    , getGauge
    , getLabel
    , getPullGauge
    , hasCounter
    , hasGauge
    , hasLabel
    , hasPullGauge
    , newRegistry
    , Counter
    , Gauge
    , Label
    , PullGauge
    , Ref
    ) where

import System.Remote.Gauge
import System.Remote.Counter
import System.Remote.Label
import System.Remote.PullGauge
import System.Remote.Registry.Internal
import qualified Data.Text as T
import qualified Data.HashMap.Strict as M
import Data.IORef (IORef, readIORef)

-- | Return the counter associated with the given name and registry.
-- Multiple calls to 'getCounter' with the same arguments will return
-- the same counter.  The first time 'getCounter' is called for a
-- given name and registry, a new, zero-initialized counter will be
-- returned.
getCounter :: T.Text  -- ^ Counter name
           -> Registry  -- ^ Registry that contains the counter
           -> IO Counter
getCounter name registry = getRef name (userCounters registry)

-- | Return the gauge associated with the given name and registry.
-- Multiple calls to 'getGauge' with the same arguments will return
-- the same gauge.  The first time 'getGauge' is called for a given
-- name and registry, a new, zero-initialized gauge will be returned.
getGauge :: T.Text  -- ^ Gauge name
         -> Registry  -- ^ Registry that contains the gauge
         -> IO Gauge
getGauge name registry = getRef name (userGauges registry)

-- | Return the label associated with the given name and registry.
-- Multiple calls to 'getLabel' with the same arguments will return
-- the same label.  The first time 'getLabel' is called for a given
-- name and registry, a new, empty label will be returned.
getLabel :: T.Text  -- ^ Label name
         -> Registry  -- ^ Registry that contains the label
         -> IO Label
getLabel name registry = getRef name (userLabels registry)

-- | Return the pullGauge associated with the given name and registry.
-- Multiple calls to 'getPullGauge' with the same arguments will return
-- the same pullGauge.  The first time 'getPullGauge' is called for a given
-- name and registry, a new, empty pullGauge will be returned.
getPullGauge :: T.Text -- ^ PullGauge name
		 -> Registry -- ^ Registry that contains the pullGauge		 
		 -> IO PullGauge
getPullGauge name registry = getRef name (userPullGauges registry)	

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

hasRef :: Ref r t => T.Text
       -> IORef (M.HashMap T.Text (r))
       -> IO Bool
hasRef name ioref = do
                cs <- readIORef ioref
                return $ M.member name cs
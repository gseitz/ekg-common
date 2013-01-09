{-# LANGUAGE CPP, ExistentialQuantification, OverloadedStrings, RecordWildCards,
  FunctionalDependencies #-}
-- | This module provides remote monitoring of a running process.
module System.Remote.Ekg
    (
      -- * Required configuration
      -- $configuration

      -- * REST API
      -- $api

      -- * The monitoring server
      -- * User-defined counters, gauges, and labels
      -- $userdefined
      getCounter
    , getGauge
    , getLabel
    , newPullGauge
    , Label
    , Counter
    , Gauge
    , PullGauge
    , Registry
    , newRegistry
    , Stats(..)
    , takeSnapshot
    , initializeBuiltInStats
    , Json
    ) where

import Control.Monad (forM)
import Data.Aeson.Types ((.=))
import qualified Data.Aeson.Types as A
import qualified Data.HashMap.Strict as M
import Data.IORef (IORef, readIORef)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Prelude hiding (read)

import System.Remote.Registry
import qualified System.Remote.Registry.Internal as Registry
import System.Remote.GHC

-- $configuration
--
-- To use this module you must first enable GC statistics collection
-- in the run-time system.  To enable GC statistics collection, either
-- run your program with
--
-- > +RTS -T
--
-- or compile it with
--
-- > -with-rtsopts=-T
--
-- The runtime overhead of @-T@ is very small so it's safe to always
-- leave it enabled.

-- $api
-- Counters, gauges and labels are stored as attributes of the
-- returned JSON objects, one attribute per counter, gauge or label.
-- In addition to user-defined counters, gauges, and labels, the below
-- built-in counters and gauges are also returned.  Furthermore, the
-- top-pullGauge JSON object of any resource contains the
-- @server_timestamp_millis@ attribute, which indicates the server
-- time, in milliseconds, when the sample was taken.
--
-- Built-in counters:
--
-- [@bytes_allocated@] Total number of bytes allocated
--
-- [@num_gcs@] Number of garbage collections performed
--
-- [@num_bytes_usage_samples@] Number of byte usage samples taken
--
-- [@cumulative_bytes_used@] Sum of all byte usage samples, can be
-- used with @numByteUsageSamples@ to calculate averages with
-- arbitrary weighting (if you are sampling this record multiple
-- times).
--
-- [@bytes_copied@] Number of bytes copied during GC
--
-- [@mutator_cpu_seconds@] CPU time spent running mutator threads.
-- This does not include any profiling overhead or initialization.
--
-- [@mutator_wall_seconds@] Wall clock time spent running mutator
-- threads.  This does not include initialization.
--
-- [@gc_cpu_seconds@] CPU time spent running GC
--
-- [@gc_wall_seconds@] Wall clock time spent running GC
--
-- [@cpu_seconds@] Total CPU time elapsed since program start
--
-- [@wall_seconds@] Total wall clock time elapsed since start
--
-- Built-in gauges:
--
-- [@max_bytes_used@] Maximum number of live bytes seen so far
--
-- [@current_bytes_used@] Current number of live bytes
--
-- [@current_bytes_slop@] Current number of bytes lost to slop
--
-- [@max_bytes_slop@] Maximum number of bytes lost to slop at any one time so far
--
-- [@peak_megabytes_allocated@] Maximum number of megabytes allocated
--
#if MIN_VERSION_base(4,6,0)
-- [@par_tot_bytes_copied@] Number of bytes copied during GC, minus
-- space held by mutable lists held by the capabilities.  Can be used
-- with 'parMaxBytesCopied' to determine how well parallel GC utilized
-- all cores.
--
-- [@par_avg_bytes_copied@] Deprecated alias for
-- @par_tot_bytes_copied@.
#else
-- [@par_avg_bytes_copied@] Number of bytes copied during GC, minus
-- space held by mutable lists held by the capabilities.  Can be used
-- with 'parMaxBytesCopied' to determine how well parallel GC utilized
-- all cores.
#endif
--
-- [@par_max_bytes_copied@] Sum of number of bytes copied each GC by
-- the most active GC thread each GC.  The ratio of
#if MIN_VERSION_base(4,6,0)
-- 'parTotBytesCopied' divided by 'parMaxBytesCopied' approaches 1 for
#else
-- 'parAvgBytesCopied' divided by 'parMaxBytesCopied' approaches 1 for
#endif
-- a maximally sequential run and approaches the number of threads
-- (set by the RTS flag @-N@) for a maximally parallel run.

------------------------------------------------------------------------
-- * User-defined counters, gauges and labels

-- $userdefined
-- A registry can store user-defined,
-- integer-valued counters, gauges and pullGauges, and string-value labels.  A
-- counter is a monotonically increasing value (e.g. TCP connections
-- established since program start.) A gauge is a variable value
-- (e.g. the current number of concurrent connections.) A label is a
-- free-form string value (e.g. exporting the command line arguments
-- or host name.) A pullGauge is a variable value that is updated automatically
-- each time it is observed (e.g. 'IO Int' for looking up the size of a 
-- 'Data.Map' inside an 'IORef'). Each counter, gauge, pullGauge and label is
-- associated with a name, which is used when it is displayed in the UI
-- or returned in a JSON object.
--
-- Even though it's technically possible to have a counter and a gauge
-- with the same name, associated within the same registry, it's not
-- recommended as it might make it harder for clients to distinguish
-- the two.
--
-- To create and use a counter, simply call 'getCounter' to create it
-- and then call e.g. 'System.Remote.Counter.inc' or
-- 'System.Remote.Counter.add' to modify its value.  Example:
--
-- > main = do
-- >     registry <- newRegistry
-- >     counter <- getCounter "iterations" registry
-- >     let loop n = do
-- >             inc counter
-- >             loop
-- >     loop
--
-- To create a gauge, use 'getGauge' instead of 'getCounter' and then
-- call e.g. 'System.Remote.Gauge.set' or
-- 'System.Remote.Gauge.modify'.  Similar for labels.

-- | All the stats exported by a registry together with a timestamp.
data Stats = Stats {
    counters  :: ![(T.Text, Json)],       -- Counters
    gauges    :: ![(T.Text, Json)],       -- Gauges
    labels    :: ![(T.Text, Json)],       -- Labels
    pullGauges    :: ![(T.Text, Json)],       -- PullGauges
    timestamp ::  {-# UNPACK #-} !Double  -- Milliseconds since epoch
  }

instance A.ToJSON Stats where
    toJSON (Stats counters gauges labels pullGauges t) = A.object $ 
        [ "server_timestamp_millis"  .= t
        , "counters"                 .= Assocs counters
        , "gauges"                   .= Assocs gauges
        , "labels"                   .= Assocs labels
        , "pull_gauges"              .= Assocs pullGauges
        ]

newtype Assocs = Assocs [(T.Text, Json)]
instance A.ToJSON Assocs where
    toJSON (Assocs xs) = A.object $ map (uncurry (.=)) xs

readAllRefs :: (Ref r t, A.ToJSON t) => IORef (M.HashMap T.Text r)
            -> IO [(T.Text, Json)]
readAllRefs mapRef = do
    m <- readIORef mapRef
    forM (M.toList m) $ \ (name, ref) -> do
        val <- Registry.read ref
        return (name, Json val)
{-# INLINABLE readAllRefs #-}

------------------------------------------------------------------------
-- | Get a snapshot of all values.  Note that we're not guaranteed to
-- see a consistent snapshot of the whole map.
takeSnapshot :: Registry -> IO Stats
takeSnapshot reg = do
    counters <- readAllRefs $ Registry.userCounters reg
    gauges   <- readAllRefs $ Registry.userGauges reg
    labels   <- readAllRefs $ Registry.userLabels reg
    pullGauges   <- readAllRefs $ Registry.userPullGauges reg
    time     <- getTimeMillis
    return $ Stats counters gauges labels pullGauges time
    

-- Existential wrapper used for OO-style polymorphism.
data Json = forall a. A.ToJSON a => Json a

instance A.ToJSON Json where
    toJSON (Json x) = A.toJSON x

------------------------------------------------------------------------
-- Utilities for working with timestamps

-- | Return the number of milliseconds since epoch.
getTimeMillis :: IO Double
getTimeMillis = (realToFrac . (* 1000)) `fmap` getPOSIXTime

{-# LANGUAGE CPP, RecordWildCards, OverloadedStrings #-}
module System.Remote.GHC
	(
		initializeBuiltInStats
	) where


import qualified GHC.Stats as Stats

import Data.Ekg.Registry

emptyGCStats :: Stats.GCStats
emptyGCStats = Stats.GCStats
    { bytesAllocated         = 0
    , numGcs                 = 0
    , maxBytesUsed           = 0
    , numByteUsageSamples    = 0
    , cumulativeBytesUsed    = 0
    , bytesCopied            = 0
    , currentBytesUsed       = 0
    , currentBytesSlop       = 0
    , maxBytesSlop           = 0
    , peakMegabytesAllocated = 0
    , mutatorCpuSeconds      = 0
    , mutatorWallSeconds     = 0
    , gcCpuSeconds           = 0
    , gcWallSeconds          = 0
    , cpuSeconds             = 0
    , wallSeconds            = 0
#if MIN_VERSION_base(4,6,0)
    , parTotBytesCopied      = 0
#else
    , parAvgBytesCopied      = 0
#endif
    , parMaxBytesCopied      = 0
    }

getGcStats :: IO Stats.GCStats
getGcStats =
#if MIN_VERSION_base(4,6,0)
    do
        enabled <- Stats.getGCStatsEnabled
        if enabled
            then Stats.getGCStats
            else return emptyGCStats
#else
    Stats.getGCStats
#endif

initializeBuiltInStats :: Registry -> IO ()
initializeBuiltInStats reg = do
    intLvl Stats.bytesAllocated "bytes_allocated"
    intLvl Stats.numGcs "num_gcs"
    intLvl Stats.parMaxBytesCopied "par_max_bytes_copied"
    intLvl Stats.numByteUsageSamples "num_bytes_usage_samples"
    intLvl Stats.cumulativeBytesUsed "cumulative_bytes_used"
    intLvl Stats.cumulativeBytesUsed "bytes_copied"
    intLvl Stats.cumulativeBytesUsed "current_bytes_used"
    intLvl Stats.cumulativeBytesUsed "current_bytes_slop"
    intLvl Stats.maxBytesUsed "max_bytes_used"
    intLvl Stats.maxBytesSlop "max_bytes_slop"
    intLvl Stats.peakMegabytesAllocated "peak_megabytes_allocated"
    doubleLvl Stats.mutatorCpuSeconds "mutator_cpu_seconds"
    doubleLvl Stats.mutatorWallSeconds "mutator_wall_seconds"
    doubleLvl Stats.gcCpuSeconds "gc_cpu_seconds"
    doubleLvl Stats.gcWallSeconds "gc_wall_seconds"
    doubleLvl Stats.cpuSeconds "cpu_seconds"
    doubleLvl Stats.wallSeconds "wall_seconds"
#if MIN_VERSION_base(4,6,0)
    intLvl Stats.parTotBytesCopied "par_tot_bytes_copied"
    intLvl Stats.parTotBytesCopied "par_avg_bytes_copied"
#else
    intLvl Stats.parAvgBytesCopied "par_avg_bytes_copied"
#endif
    intLvl Stats.parMaxBytesCopied "par_max_bytes_copied"
    return ()
  where
    intLvl f name = newPullGauge name reg $ fmap (fromIntegral . f) getGcStats
    doubleLvl f name = newPullGauge name reg $ fmap (fromIntegral . round . f) getGcStats

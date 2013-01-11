module Data.Ekg.Histogram.ExpDecayingSample
	(

	) where



--data ExpDecayingSample = ExpDecayingSample {
--                          reservoirSize :: Int
--                        , alpha :: Double
--                        , count :: Integer
--                        , startTime :: Integer
--                        , nextScaletime :: Integer
--                        , values :: M.HashMap Double Integer
--                    }


--newExpDecayingSample :: Int -> Double -> Integer -> ExpDecayingSample
--newExpDecayingSample size alpha currentTime = ExpDecayingSample size alpha 0 currentTime 0 M.empty


--weight sample x = exp (alpha sample * x)

---- adjust :: (Hashable k, Ord k) => (a -> a) -> k -> Map k a -> Map k a

--rescale :: Integer -> Integer -> Integer -> ExpDecayingSample -> ExpDecayingSample
--rescale timeInSeconds now next sample = adjustedSample
--    where oldStartTime = startTime sample
--          transform (k, v) = (k * exp (- alpha sample) * (fromInteger timeInSeconds -  fromInteger oldStartTime), v)
--          tuples = toList $ values sample
--          adjusted = fromList $ fmap (transform) tuples
--          adjustedSample = sample { values = adjusted, startTime = oldStartTime, count = toInteger $ M.size adjusted }


--instance Sample ExpDecayingSample where
--    size sample = min (count sample) (toInteger $ reservoirSize sample)
--    update sample value timestamp randDouble = adjustedSample
--        where priority = weight sample (timestamp - startTime sample) / randDouble
--              newCount = count sample + 1
--              vals = values sample
--              adjustedSample = if newCount <= reservoirSize sample
--                  then sample { count = newCount, values = insert priority value vals }
--                  else updated
--                      where firstKey valmap = min $ keys valmap


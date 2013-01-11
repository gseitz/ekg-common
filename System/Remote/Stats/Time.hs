module System.Remote.Stats.Time
	(
      TimeUnit(..)
    , toNanos
    , toMicros
    , toMillis
    , toSeconds
    , toMinutes
    , toHours
    , toDays
	) where

data TimeUnit = Day
              | Hour
              | Minute
              | Second
              | Millisecond
              | Microsecond
              | Nanosecond

ns :: Integer
ns = 1
us :: Integer
us = ns * 1000
ms :: Integer
ms = us * 1000
s :: Integer
s  = ms * 1000
m :: Integer
m  = s * 60
h :: Integer
h  = m * 60
d :: Integer
d  = h * 24

toNanos :: TimeUnit  -- ^ Source unit
        -> Integer   -- ^ Value
        -> Integer   -- ^ Result
toNanos Nanosecond  = id
toNanos Microsecond = ( * (us // ns))
toNanos Millisecond = ( * (ms // ns))
toNanos Second      = ( * ( s // ns))
toNanos Minute      = ( * ( m // ns))
toNanos Hour        = ( * ( h // ns))
toNanos Day         = ( * ( d // ns))


toMicros :: TimeUnit  -- ^ Source unit
        -> Integer   -- ^ Value
        -> Integer   -- ^ Result
toMicros Nanosecond  = (// (us // ns))
toMicros Microsecond = id
toMicros Millisecond = ( * (ms // us))
toMicros Second      = ( * ( s // us))
toMicros Minute      = ( * ( m // us))
toMicros Hour        = ( * ( h // us))
toMicros Day         = ( * ( d // us))


toMillis :: TimeUnit  -- ^ Source unit
        -> Integer   -- ^ Value
        -> Integer   -- ^ Result
toMillis Nanosecond  = (// (ms // ns))
toMillis Microsecond = (// (ms // us))
toMillis Millisecond = id
toMillis Second      = ( * ( s // ms))
toMillis Minute      = ( * ( m // ms))
toMillis Hour        = ( * ( h // ms))
toMillis Day         = ( * ( d // ms))


toSeconds :: TimeUnit  -- ^ Source unit
        -> Integer   -- ^ Value
        -> Integer   -- ^ Result
toSeconds Nanosecond  = (// ( s // ns))
toSeconds Microsecond = (// ( s // us))
toSeconds Millisecond = (// ( s // ms))
toSeconds Second      = id
toSeconds Minute      = ( * ( m //  s))
toSeconds Hour        = ( * ( h //  s))
toSeconds Day         = ( * ( d //  s))


toMinutes :: TimeUnit  -- ^ Source unit
        -> Integer   -- ^ Value
        -> Integer   -- ^ Result
toMinutes Nanosecond  = (// ( m // ns))
toMinutes Microsecond = (// ( m // us))
toMinutes Millisecond = (// ( m // ms))
toMinutes Second      = (// ( m //  s))
toMinutes Minute      = id
toMinutes Hour        = ( * ( h //  m))
toMinutes Day         = ( * ( d //  m))

toHours :: TimeUnit  -- ^ Source unit
        -> Integer   -- ^ Value
        -> Integer   -- ^ Result
toHours Nanosecond  = (// ( h // ns))
toHours Microsecond = (// ( h // us))
toHours Millisecond = (// ( h // ms))
toHours Second      = (// ( h //  s))
toHours Minute      = (// ( h //  m))
toHours Hour        = id
toHours Day         = ( * ( d //  h))


toDays :: TimeUnit  -- ^ Source unit
        -> Integer   -- ^ Value
        -> Integer   -- ^ Result
toDays Nanosecond  = (// ( d // ns))
toDays Microsecond = (// ( d // us))
toDays Millisecond = (// ( d // ms))
toDays Second      = (// ( d //  s))
toDays Minute      = (// ( d //  m))
toDays Hour        = (// ( d // h))
toDays Day         = id

(//) :: Integral a => a -> a -> a
(//) = div
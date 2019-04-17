import Data.TCache.Memoization
import Data.TCache.DefaultPersistence
import Control.Concurrent
import System.Time

-- | memoization caches a value for a given amount of time
--   This demo stores the current time for 4 seconds until
--   it generates the next timestamp

main= do
        cachedByKey "timequant" 4 f >>= print
        threadDelay 1000000
        main

f= do
  TOD t _ <- getClockTime
  return t

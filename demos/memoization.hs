import Data.TCache.Memoization
import Data.TCache.DefaultPersistence
import Control.Concurrent
import System.Time



main= do
        cachedByKey "" 4 f >>= print
        threadDelay 1000000
        main

f= do
  TOD t _ <- getClockTime
  return t

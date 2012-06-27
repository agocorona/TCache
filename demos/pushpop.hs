
import Data.Persistent.Collection
import Control.Concurrent
import Data.TCache

main= do
  let x= "a"
  let q= getQRef "hi"
  forkIO $ pop q >>= print
--  forkIO $ pop q >>= print
  push q x
  push q x
  syncCache

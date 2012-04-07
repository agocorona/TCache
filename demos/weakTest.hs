import System.Mem.Weak
import Control.Concurrent
import Debug.Trace

debug= flip trace

dat= "this is the data"

main= do
  mkWeakPtr  dat . Just $ print "deleted" `debug` "deleted"

  main

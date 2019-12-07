import System.Mem.Weak
import Debug.Trace

debug :: c -> String -> c
debug = flip trace

dat :: String
dat = "this is the data"

main :: IO b
main = do
  _ <- mkWeakPtr  dat . Just $ print "deleted" `debug` "deleted"
  main

module Main where
import Control.Workflow
import Control.Concurrent(threadDelay)
import System.IO (hFlush,stdout)

printLine x= do
   putStr (show x ++ " ")
   hFlush stdout
   threadDelay 100000


mcount :: Int -> Workflow IO ()
mcount n= do
             if n==5
              then return ()
              else do
                step $  printLine n
                mcount (n+1)



main=do
  startWF  "count" 0  [("count",mcount)]
  startWF  "count"  0  [("count",mcount)]
  startWF  "count" 0   [("count",mcount)]
  threadDelay 20000000
  startWF  "count" 0   [("count",mcount)]

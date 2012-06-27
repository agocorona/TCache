{-# OPTIONS -XFlexibleInstances

            #-}

import Data.Persistent.Collection
import Data.TCache
import Control.Concurrent

quser doc  = getQRef doc

separator= print "----------------"

qdocApprobal doc  = getQRef doc

main= do
  let readq who= forkIO $ pick  (quser who) >>= \doc -> pop (qdocApprobal   doc)  >>= \x -> print (x :: Bool)
  putStrLn "boss1 1\r\nboss2 2\r\n"
  c <- getLine
  case read c of
    1 -> readq "boss1" >> aprobal "boss1"
    2 -> readq "boss2" >> aprobal "boss2"

aprobal who= do
 separator
 aprobalList
 putStrLn $ "thanks , press any key to exit, "++ who
 getLine
 syncCache
 return ()
 where
 quserwho= quser who
 aprobalList= do
     empty <- isEmpty  (quserwho)
     if empty
         then   do
            putStrLn  "No more document to validate. Bye"

            return ()
         else do
             rdoc <- pick  (quserwho)

             approbal1 rdoc
             aprobalList
 approbal1 :: String -> IO ()
 approbal1 doc= do
       putStrLn $ "hi " ++ who ++", a new request for aprobal has arrived:"

       print doc
       putStrLn $  "Would you approbe this document? s/n"
       l <-    getLine
       if l/= "s" && l /= "n" then approbal1 doc else do
        let b= head l
        let res= if b == 's' then  True else  False
           -- send the message to the workflow
        atomically $ do
                popSTM   (quserwho)
                pushSTM  (qdocApprobal   doc)  res

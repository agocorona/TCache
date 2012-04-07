{-# LANGUAGE DeriveDataTypeable #-}
module Main where
-------------------------------------------------
-- TCache example
-- it ilustrates the use of clearSyncCacheProc while the data is being updated
------------------------------------------------

import Data.TCache
import Data.TCache.DefaultPersistence
import Data.ByteString.Lazy.Char8(pack,unpack)
import Control.Concurrent
import Debug.Trace
import Data.Typeable
debug a b= trace b a

-- The data elements to be used in the example

data  Data=   Data Int Int deriving (Read, Show, Typeable)



instance Indexable Data where
        key         (Data i _)= show i
        defPath _ = "cacheData/"  -- directory where the data is stored.

instance Serializable Data where
  serialize= pack . show
  deserialize= read . unpack


main=  do

        putStrLn "see the source code of this example"
        putStrLn "This program test the caching and cleaning and re-retrieval and update of the cache"

        putStrLn "asyncronous write every 10 seconds, 100 elems max cache size"
        putStrLn "default policy (defaultCheck) for clearing the cache is to reduce the cache to half of max sixe when size exceeds the max"

        putStrLn ""
        putStrLn "create resources"
        putStrLn " (acces no resources and return two new Data objects defined in items)"
        withResources[] $ const[Data i 0 | i <- [1..200]]

        putStrLn ""
        clearSyncCacheProc  10 defaultCheck 100
        putStrLn $ "every 10 seconds, the modified data in the cache is written in the folder: " ++  defPath ( undefined :: Data)
        putStrLn "wait 10 seconds  to let the next write cycle to enter (every 10 seconds, set by clearSyncCacheProc)"


        putStrLn "because 200 exceeds the maximum cache size (100) defaultCheck will discard the 150  older elems  to reduce the cache to a half"
        putStrLn "This is the behaviour defined in defaultCheck."
        threadDelay 20000000
        putStrLn " update every element, included the discarded ones"
        withResources [Data i undefined | i <- [1..200]] $
          \ds ->  [ Data i (n+1)  | Just(Data i n) <- ds]

        putStrLn $"wait for the next cycle of file update. The files must contain 1 instead o 0 (Data n 1) in the folder "++ defPath ( undefined :: Data)
        threadDelay 20000000


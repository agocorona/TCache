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

debug :: a -> String -> a
debug a b= trace b a

-- The data elements to be used in the example

data  Data=   Data Int Int deriving (Read, Show, Typeable)



instance Indexable Data where
        key         (Data i _)= show i
        defPath _ = ".tcachedata/caching/"  -- directory where the data is stored.

instance Serializable Data where
  serialize= pack . show
  deserialize= read . unpack


printStat :: (Show a1, Show a2, Show a3) => (a1, a2, a3) -> IO ()
printStat (total, dirty, loaded) =
  putStrLn $ "total: " ++ show total ++ " dirty: " ++ show dirty ++ " loaded: " ++ show loaded

main :: IO ()
main =  do
        putStrLn "See the source code of this example!"
        putStrLn ""
        putStrLn "This program tests the caching, cleaning, re-retrieval and updating of the cache."
        putStrLn "It uses the DefaultPersistence (disk) and defaultCheck (cleaning rules)."
        putStrLn "It writes asyncronously every 10 seconds all changed elemements to disk."
        putStrLn "When there is more than the allowed number of elements (100) in the cache it cleans them by the given rule."
        putStrLn "With defaultCheck it drops elements which where not accesed since half the time between now and the last sync."

        putStrLn ""
        putStrLn "Creating 200 resources with content: n 0"
        withResources[] $ const[Data i 0 | i <- [1..200]]
        -- get stats about them (total, dirty, loaded)
        statElems >>= printStat

        x1 <- getResources [Data i 0 | i <- [1..200]]
        putStrLn $ "Last element: " ++ show (last x1)

        putStrLn ""
        putStrLn $ "Starting the async proc with folder: " ++  defPath ( undefined :: Data)
        _ <- clearSyncCacheProc 10 defaultCheck 100
        threadDelay 6000000

        putStrLn "after 6 seconds"
        statElems >>= printStat
        threadDelay 5000000

        putStrLn "after 11 seconds (should have saved)"
        statElems >>= printStat
        threadDelay 5000000

        putStrLn "after 16 seconds (accessing one element)"
        -- I read (access) all the data here!
        getResource (Data 100 undefined) >>= print

        statElems >>= printStat
        --syncCache
        threadDelay 5000000

        putStrLn "after 21 seconds (should have cleaned)"
        statElems >>= printStat

        putStrLn "Updating every element, included the discarded ones with 'n 1'"
        withResources [Data i undefined | i <- [1..200]] $
          \ds ->  [ Data i (n+1)  | Just(Data i n) <- ds]
        threadDelay 5000000

        putStrLn "after 26 seconds (should be 'full')"
        statElems >>= printStat

        putStrLn "accessing all entries once and print the last"
        -- I read (access) all the data here!
        x2 <- getResources [Data i 1 | i <- [1..200]]
        print $ last x2

        threadDelay 5000000

        putStrLn "after 31 seconds (should have saved)"
        statElems >>= printStat
        threadDelay 5000000

        putStrLn "after 36 seconds"
        statElems >>= printStat
        threadDelay 5000000

        putStrLn "after 41 seconds (should be cleaned again)"
        statElems >>= printStat

        -- reloading all of the data again
        putStrLn "getting the first 50 elements"
        x <- getResources [Data i 1 | i <- [1..50]]
        putStrLn $ "Last element: " ++ show (last x)

        putStrLn "Now we have"
        statElems >>= printStat

{-# OPTIONS -XFlexibleInstances

            #-}

import Data.TCache
import Data.TCache.DefaultPersistence
import Data.ByteString.Lazy.Char8
import System.Mem.Weak
import Debug.Trace

main= do
   mapM (\n -> mkWeakPtr  "hi" . Just $ trace "deelte" print (show n))$ Prelude.take 1000 [1..]


   hist <- getLine
   withResource []   $ \ ms -> add hist  ms
   let r=  refcache
--   Just l <- getResource ([] :: [String])
--   Just l2 <- atomically $ readDBRef ref
--   print  l
--   print l2
   main



ref= getDBRef "index" :: DBRef [String]

add h (Just stories)= h:stories
add h Nothing =  [h]

instance Indexable [String] where
   key= const "index"

instance Serializable [String] where
   serialize= pack . show
   deserialize= read . unpack

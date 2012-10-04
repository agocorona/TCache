{-# OPTIONS -XDeriveDataTypeable
            #-}
module Main where
import Data.TCache
import Data.Typeable
import Data.TCache.DefaultPersistence
import Data.ByteString.Lazy.Char8
import Control.Concurrent(threadDelay)
import System.IO (hFlush,stdout)

data Ops= Plus | Times deriving (Read, Show, Typeable)

instance Serializable Ops where
  serialize= pack . show
  deserialize= read . unpack

instance Indexable Ops where
 key _ = "ops"

main= do
   let ref = getDBRef $ keyResource Times
   atomically $ writeDBRef ref Plus
   syncCache


   print ref

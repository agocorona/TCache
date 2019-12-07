module Main where
import Data.TCache
import Data.Typeable
import Data.TCache.DefaultPersistence
import Data.ByteString.Lazy.Char8

data Ops= Plus | Times deriving (Read, Show, Typeable)

instance Serializable Ops where
  serialize= pack . show
  deserialize= read . unpack

instance Indexable Ops where
 key _ = "ops"

main :: IO ()
main = do
    let ref = getDBRef $ keyResource Times
    atomically $ writeDBRef ref Plus
    syncCache


    print ref

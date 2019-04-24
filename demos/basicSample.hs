{-# LANGUAGE  DeriveDataTypeable #-}
module Main where


import Data.TCache
import Data.TCache.DefaultPersistence
import Data.ByteString.Lazy.Char8(pack,unpack)
import Control.Concurrent
import Data.Foldable (for_)
import Data.Typeable
import Debug.Trace

debug :: a -> String -> a
debug a b= trace b a

-- The data elements to be used in the example: A user will repeatedly buy Items.

data  Data=   User{uname::String, uid::String, spent:: Int} |
              Item{iname::String, iid::String, price::Int, stock::Int}

              deriving (Read, Show, Typeable)


-- defining prototypes to make missing-fields warning useful again
user_ :: Data
user_ = User{uname = undefined, uid = undefined, spent = undefined }

item_ :: Data
item_ = Item{iname = undefined, iid = undefined, price = undefined, stock = undefined }

-- The mappings between the cache and the physical storage are defined by the interface IResource
--      to extract the unique key,
--      to serializa to string
--      to deserialize from string
--      key prefix (or , if use "/", directory where to store the resource if default write is to be used)
--      to read the resource from the physical storage, (optional, default in files)
--      to store it  (optional, default from file)
--      to delete the resource from the physical storage. (optional. Default provided)



instance Indexable Data where
  key   User { uid=id' } = id'
  key   Item { iid=id' } = id'

instance Serializable Data where
  serialize = pack . show
  deserialize = read . unpack



-- buy is the operation to be performed in the example

-- withResources gets a partial definition of each resource necessary for extracting the key,
-- fill all the rest of the data structures (if found ) and return a list of Maybe Data.

-- buyIt is part of the domain problem. it receive this list and generates a new list of
-- data objects that are updated in the cache. buyIt is executed atomically.


buy :: Data -> Data -> IO ()
user `buy` item =  withResources [user, item] buyIt
 where
    buyIt[Just us, Just it]
       | stock it > 0 = [us',it'] `debug` ("john spent " ++ show (spent us) ++
          " so far. Hey tries to buy a PC from the stock of " ++ show (stock it))
       | otherwise   = error "stock is empty for this product"

      where
       us'= us{ spent = spent us + price it}
       it'= it{ stock = stock it - 1 }

    buyIt _ = error "either the user or the item does not exist"


main :: IO ()
main = do
  -- create resources (access no resources and return two new Data objects defined in items)
  withResources [] prepareItems

  -- 11 PCs are charged to the John´s account in parallel, to show transactionality
  -- because there are only 10 PCs in stock, the last thread must return an error
  for_ [(1::Int)..11] $ const $ forkIO $ user_{ uid = "U12345" } `buy` item_{ iid = "I54321" }

  -- wait a second (to let the forked io finish)
  threadDelay 1000000

  -- get the contents of the resources by their keys
  [us,it] <-  getResources [user_{ uid = "U12345" }, item_{ iid = "I54321" }]

  putStrLn $  "user data=" ++ show us
  putStrLn $  "item data=" ++ show it

  -- write the cache content in a persistent store (invoke writeResource for each resource)
  -- in a real application clearSyncCacheProc can be used instead to adjust size and write the cache periodically
  syncCache
  threadDelay 1000000

  -- the files U12345 and I54321 in .tcachedata must now contain the result of the 11 iterations

  where
    prepareItems = const
      [ User "John" "U12345" 0
      , Item "PC" "I54321" 6000 10
      ]

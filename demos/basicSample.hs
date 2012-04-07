{-# LANGUAGE  DeriveDataTypeable #-}
module Main where


import Data.TCache
import Data.TCache.DefaultPersistence
import Data.ByteString.Lazy.Char8(pack,unpack)
import Control.Concurrent
import Data.Typeable
import Debug.Trace

debug a b= trace b a

-- The data elements to be used in the example: A user will repeatedly buy Items.

data  Data=   User{uname::String, uid::String, spent:: Int} |
              Item{iname::String, iid::String, price::Int, stock::Int}

              deriving (Read, Show, Typeable)


-- The mappings between the cache and the phisical storage are defined by the interface IResource
--      to extract the unique key,
--      to serializa to string
--      to deserialize from string
--      key prefix (or , if use "/", directory where to store the resource if default write is to be used)
--      to read the resource from the physical storage, (optional, default in files)
--      to store it  (optional, default from file)
--      to delete the resource from the physical storage. (optional. Default provided)



instance Indexable Data where
        key   User{uid=id}= id
        key   Item{iid=id}= id

instance Serializable Data where
  serialize= pack . show
  deserialize= read . unpack



-- buy is the operation to be performed in the example

--withResources gets a partial definition of each resource necessary for extracting the key,
--fill all the rest of the data structures (if found ) and return a list of Maybe Data.
--BuyIt is part of the domain problem. it receive this list and generates a new list of
--data objects that are updated in the cache. buyIt is executed atomically.


user `buy` item=  withResources[user,item] buyIt
 where
    buyIt[Just us,Just it]
       | stock it > 0= [us',it']   `debug` "john buy a PC"
       | otherwise   = error "stock is empty for this product"

      where
       us'= us{spent=spent us + price it}
       it'= it{stock= stock it-1}

    buyIt _ = error "either the user or the item does not exist"


main= do
        -- create resources (acces no resources and return two new Data objects defined in items)
        withResources[]items

        --11 PCs are charged  to the John´s account in paralel, to show transactionality
        --because there are only 10 PCs in stock, the last thread must return an error

        for 11 $ forkIO $ User{uid="U12345"} `buy` Item{iid="I54321"}

        --wait 1 seconds
        threadDelay 1000000

        [us,it] <-  getResources [User{uid="U12345"}, Item{iid="I54321"}]

        putStrLn $  "user data=" ++ show us
        putStrLn $  "item data=" ++ show it

        -- write the cache content in a persistent store (invoque writeResource for each resource)
        -- in a real application clearSyncCacheProc can be used instead to adjust size and write the cache periodically

        syncCache
        threadDelay 1000000

        -- the files have been created. the files U12345 and I54321 must contain the result of the 11 iterations

  where
        items _=
              [User "John" "U12345" 0
              ,Item "PC" "I54321" 6000 10]

        for 0 _ = return ()
        for n f= f >> for (n-1) f

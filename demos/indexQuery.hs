{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, UndecidableInstances #-}
module Main where
import Data.TCache
import Data.TCache.IndexQuery
import Data.TCache.DefaultPersistence
import Data.ByteString.Lazy.Char8(pack,unpack)
import Debug.Trace

import Data.Typeable


data Person= Person {pname :: String, age :: Int} deriving  (Show, Read, Eq, Typeable)

data Car= Car{owner :: DBRef Person , cname:: String} deriving (Show, Read, Eq, Typeable)

instance Indexable Person where key Person{pname= n} = "Person " ++ n
instance Indexable Car where key Car{cname= n} = "Car " ++ n

instance (Read a, Show a) => Serializable a where
   serialize= pack . show
   deserialize= read . unpack

main =  do

   index owner
   index pname
   index cname
   index age

   bruce <- atomically $    newDBRef $ Person "bruce" 42
   atomically $  mapM_ newDBRef [Car bruce "Bat Mobile", Car bruce "Porsche"]

   r <- atomically $ cname .>=. "Bat Mobile"
   print r

   r <- atomically $ select (cname, owner) $  (owner .==. bruce)  .&&. (cname .==. "Bat Mobile")
   print r

   r <- atomically $ age .>=. (20 :: Int)
   print r


   --syncCache

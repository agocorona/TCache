{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, UndecidableInstances #-}
module Main where
import Data.TCache
import Data.TCache.IndexQuery
import Data.TCache.DefaultPersistence
import Data.ByteString.Lazy.Char8(pack,unpack)
import Data.Typeable

data Person= Person {pname :: String, age :: Int} deriving  (Show, Read, Eq, Typeable)

data Car= Car{owner :: DBRef Person , cname:: String} deriving (Show, Read, Eq, Typeable)

instance Indexable Person where key Person{pname= n} = "Person " ++ n
instance Indexable Car where key Car{cname= n} = "Car " ++ n

instance Serializable Person where
   serialize = pack . show
   deserialize = read . unpack

instance Serializable Car where
   serialize = pack . show
   deserialize = read . unpack

main :: IO ()
main =  do

   index owner
   index pname
   index cname
   index age

   bruce <- atomically $ newDBRef $ Person "bruce" 42
   atomically $ mapM_ newDBRef [Car bruce "Bat Mobile", Car bruce "Porsche"]

   r1 <- atomically $ cname .>=. "Bat Mobile"
   print r1

   r2 <- atomically $ select (cname, owner) $  (owner .==. bruce)  .&&. (cname .==. "Bat Mobile")
   print r2

   r3 <- atomically $ age .>=. (20 :: Int)
   print r3

   --syncCache

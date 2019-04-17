{-# LANGUAGE DeriveDataTypeable #-}
module Main where
import Data.TCache
import Data.TCache.DefaultPersistence
import Data.TCache.IndexQuery(select)
import Data.TCache.IndexText
import Data.ByteString.Lazy.Char8(pack,unpack)
import qualified Data.Text.Lazy as T(pack)
import Data.Typeable

data Doc= Doc{title, body :: String} deriving (Read,Show, Typeable)
instance Indexable Doc where
  key Doc{title=t}= t

instance Serializable Doc  where
  serialize= pack . show
  deserialize= read . unpack

main= do
  indexText  body T.pack
  let doc= Doc{title=  "title", body=  "hola que tal estamos"}
  rdoc <- atomically $ newDBRef doc
  r1 <- atomically $ select title $ body `contains` "hola que tal"
  print r1

  atomically $ writeDBRef rdoc  doc{ body=  "que tal"}
  r <- atomically $ select title $ body `contains` "hola que tal"
  print r
  if  r1 == [title doc] then print "OK" else print "FAIL"
  if  null r then print "OK" else print "FAIL"

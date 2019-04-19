{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Main where
import Data.TCache
import Data.TCache.DefaultPersistence
import Data.ByteString.Lazy.Char8(pack,unpack)
import Data.Typeable

{------------- tests---------
example of IDynamic usage.

-}

-- Very simple data:
-- Two objects with two different datatypes: MyInt and MyString
-- We use newtypes so we don't need to create orphan instances

newtype MyInt = MyInt { fromMyInt :: Int } deriving ( Eq, Show, Typeable, Read )
newtype MyString = MyString { fromMyString :: String } deriving ( Eq, Show, Typeable, Read )

instance Indexable MyString where
  -- making the key 2 chars wide
  key x =  take 2 $ fromMyString x

instance Indexable MyInt where
  -- just use the string representation as key here
  key =  show

instance Serializable MyString where
  serialize = pack . show
  deserialize = read . unpack

instance Serializable MyInt where
  serialize = pack . show
  deserialize = read . unpack


main :: IO ()
main= do
  putStrLn "see the code to know the meaning of he results"

  -- NOTE: registerType no longer needed


  let x = MyInt 1

  -- now *Resources primitives support different datatypes
  -- without the need  of Data.Dynamic
  withResources  [] $ const  [x]
  withResources  [] $ const  [MyString "hola"] --resources creation

  syncCache

  res1 <- getResource  x
  print res1

  res2 <- getResource $ MyString "ho"
  print res2

  -- to use heterogeneous data in the same transaction,
  -- use DBRef's:
  s <- atomically $ do
        let refInt    = getDBRef $ key x    :: DBRef MyInt
            refString = getDBRef $ key (MyString "ho") :: DBRef MyString
        i <- readDBRef refInt
        writeDBRef refString $ MyString $ "hola, the retrieved value of x is " ++ show i
        readDBRef refString

  print s

  -- however, retrieval of data with the incorrect type will generate an exception:

  syncCache

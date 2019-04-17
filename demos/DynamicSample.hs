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
-- Two objects with two different datatypes: Int and String2
-- Notice: We need a newtype String for this example because of the special treatment of the key
-- while TCache.Defs already defines the String itself (id) as key.

newtype String2 = String2 { fromString2 :: String } deriving ( Eq, Show, Typeable, Read )

instance Indexable String2 where
  -- making the key 2 chars wide
  key x=  take 2 $ fromString2 x

instance (Read a, Show a) => Serializable a where
  serialize = pack . show
  deserialize = read . unpack


main= do
  putStrLn "see the code to know the meaning of he results"

  -- NOTE: registerType no longer needed


  let x= 1:: Int

  -- now *Resources primitives support different datatypes
  -- without the need  of Data.Dynamic
  withResources  [] $ const  [x]
  withResources  [] $ const  [String2 "hola"] --resources creation

  syncCache

  res <- getResource  x
  print res

  res <- getResource $ String2 "ho"
  print res

  -- to use heterogeneous data in the same transaction,
  -- use DBRef's:
  s <- atomically $ do
        let refInt    = getDBRef $ key x    :: DBRef Int
            refString = getDBRef $ key (String2 "ho") :: DBRef String2
        i <- readDBRef refInt
        writeDBRef refString $ String2 $ "hola, the retrieved value of x is " ++ show i
        readDBRef refString

  print s

  -- however, retrieval of data with the incorrect type will generate an exception:

  syncCache



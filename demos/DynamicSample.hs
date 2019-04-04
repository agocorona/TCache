{-# OPTIONS -XTypeSynonymInstances  -XFlexibleInstances  -XUndecidableInstances #-}
-- XTypeSynonymInstances added only to permit IResource instances for Strings
module Main where
import Data.TCache
import Data.TCache.DefaultPersistence
import Data.ByteString.Lazy.Char8(pack,unpack)
import Data.Typeable

{------------- tests---------
example of IDynamic usage.

-}

--very simple data:
--two objects with two different datatypes: Int and String
{-
instance Indexable Int where
   key =  show


instance Indexable String where
   key x=  take 2 x
-}
instance (Read a, Show a) => Serializable a where
   serialize= pack . show
   deserialize= read . unpack


main= do
  putStrLn "see the code to know the meaning of he results"

  -- NOTE: registerType no longer needed


  let x= 1:: Int

  -- now *Resources primitives suppont different datatypes
  -- without the need  of Data.Dynamic
  withResources  [] $ const  [x]
  withResources  [] $ const  ["hola"]  --resources creation

  syncCache

  res <- getResource  x
  print res

  res <- getResource  "ho"
  print res

  -- to use heterogeneous data in the same transaction,
  -- use DBRef's:
  s <- atomically $ do
        let refInt    = getDBRef $ key x    :: DBRef Int
            refString = getDBRef $ key "ho" :: DBRef String
        i <- readDBRef refInt
        writeDBRef refString $ "hola, the retrieved value of x is " ++ show i
        s <- readDBRef refString
        return s

  print s

  -- however, retrieval of data with the incorrect type will generate an exception:

  syncCache



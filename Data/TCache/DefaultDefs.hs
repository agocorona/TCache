-----------------------------------------------------------------------------
--
-- Module      :  Data.TCache.DefaultDefs
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
{- |

This module decouples the interface of 'IResource" class in two classes
one for key extraction 'Indexable' and other ('Serializable" for serlalization and persistence
.This last one defines persistence in files as default, but it can be changed
to persistence in databases, for examople.

-}
{-# LANGUAGE   FlexibleInstances, UndecidableInstances
               , MultiParamTypeClasses, FunctionalDependencies

               , ExistentialQuantification
               , ScopedTypeVariables

                #-}
module Data.TCache.DefaultDefs(Indexable(..),Serializable(..),defaultPersist,Persist(..)) where


import Data.TCache.IResource
import Data.Typeable
import System.IO.Unsafe
import Data.IORef
import System.Directory
import Control.Monad(when,replicateM)
import System.IO
import System.IO.Error
import Control.Exception as Exception
import Control.Concurrent
import Data.List(elemIndices,isInfixOf)
import Data.Maybe(fromJust)
import Data.TCache.Defs(castErr)
import qualified Data.ByteString.Lazy.Char8 as B

--import Debug.Trace
--
--a !> b = trace b a



{- | Indexable is an utility class used to derive instances of IResource

Example:

@data Person= Person{ pname :: String, cars :: [DBRef Car]} deriving (Show, Read, Typeable)
data Car= Car{owner :: DBRef Person , cname:: String} deriving (Show, Read, Eq, Typeable)
@

Since Person and Car are instances of 'Read' ans 'Show', by defining the 'Indexable' instance
will implicitly define the IResource instance for file persistence:

@
instance Indexable Person where  key Person{pname=n} = \"Person \" ++ n
instance Indexable Car where key Car{cname= n} = \"Car \" ++ n
@
-}


class Indexable a where
    key:: a -> String
    defPath :: a -> String       -- ^ additional extension for default file paths.
                                -- The default value is "data/".

                                -- IMPORTANT:  defPath must depend on the datatype, not the value (must be constant). Default is "TCacheData/"
    defPath =  const "TCacheData/"

--instance IResource a => Indexable a where
--   key x= keyResource x

{- | Serialize is an abstract serialization ionterface in order to define implicit instances of IResource.
The deserialization must be as lazy as possible if deserialized objects contain DBRefs,
lazy deserialization avoid unnecesary DBRef instantiations when they are not accessed,
since DBRefs instantiations involve extra cache lookups
For this reason serialization/deserialization is to/from ordinary Strings
serialization/deserialization are not performance critical in TCache

Read, Show,  instances are implicit instances of Serializable

>    serialize  = show
>    deserialize= read

Since write and read to disk of to/from the cache must not be very often
The performance of serialization is not critical.
-}
class Serializable a {-serialFormat-} | a -> {-serialFormat-} where
  serialize   :: a -> B.ByteString --serialFormat
  deserialize :: {-serialFormat-} B.ByteString -> a
  setPersist :: a -> Persist
  setPersist _= defaultPersist

--instance (Show a, Read a)=> Serializable a where
--  serialize= show
--  deserialize= read


-- | a persist mechanism has to implement these three primitives
-- 'defaultpersist' is the default file persistence
data Persist = Persist{
       readByKey   ::  (String -> IO(Maybe B.ByteString)) -- ^  read by key
     , write       ::  (String -> B.ByteString -> IO())   -- ^  write
     , delete      ::  (String -> IO())}       -- ^  delete

defaultPersist= Persist
    {readByKey= defaultReadByKey
    ,write= defaultWrite
    ,delete= defaultDelete}

getPersist x= return (setPersist x)
  `Exception.catch` (\(e:: SomeException) -> error "setPersist must not depend on the type, not the value of the parameter: " )




defaultReadByKey ::   String-> IO (Maybe B.ByteString)
defaultReadByKey k= iox   -- !> "defaultReadByKey"
     where
     iox = handle handler $ do   
             s <-  readFileStrict  k 
             return $ Just   s                                                       -- `debug` ("read "++ filename)

 
     handler ::  IOError ->  IO (Maybe B.ByteString)
     handler  e
      | isAlreadyInUseError e = defaultReadByKey  k                         
      | isDoesNotExistError e = return Nothing
      | otherwise= if ("invalid" `isInfixOf` ioeGetErrorString e)
         then
            error $  "readResource: " ++ show e ++ " defPath and/or keyResource are not suitable for a file path"
              
         else defaultReadByKey  k


defaultWrite :: String-> B.ByteString -> IO()
defaultWrite filename x= safeWrite filename  x
safeWrite filename str= handle  handler  $ B.writeFile filename str  -- !> ("write "++filename)
     where          
     handler e-- (e :: IOError)
       | isDoesNotExistError e=do 
                  createDirectoryIfMissing True $ take (1+(last $ elemIndices '/' filename)) filename   --maybe the path does not exist
                  safeWrite filename str               


       | otherwise= if ("invalid" `isInfixOf` ioeGetErrorString e)
             then
                error  $ "writeResource: " ++ show e ++ " defPath and/or keyResource are not suitable for a file path"
             else do
                hPutStrLn stderr $ "defaultWriteResource:  " ++ show e ++  " in file: " ++ filename ++ " retrying"
                safeWrite filename str
              
defaultDelete :: String -> IO()
defaultDelete filename =do
     handle (handler filename) $ removeFile filename
     --print  ("delete "++filename)
     where

     handler :: String -> IOException -> IO ()
     handler file e
       | isDoesNotExistError e= return ()  --`debug` "isDoesNotExistError"
       | isAlreadyInUseError e= do
            hPutStrLn stderr $ "defaultDelResource: busy"  ++  " in file: " ++ filename ++ " retrying"
--            threadDelay 100000   --`debug`"isAlreadyInUseError"
            defaultDelete filename  
       | otherwise = do
           hPutStrLn stderr $ "defaultDelResource:  " ++ show e ++  " in file: " ++ filename ++ " retrying"
--           threadDelay 100000     --`debug` ("otherwise " ++ show e)
           defaultDelete filename




-- | Strict read from file, needed for default file persistence
readFileStrict f = openFile f ReadMode >>= \ h -> readIt h `finally` hClose h
  where
  readIt h= do
      s   <- hFileSize h
      let n= fromIntegral s
      str <- B.hGet h n -- replicateM n (B.hGetChar h) 
      return str






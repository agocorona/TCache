 {-# LANGUAGE   ScopedTypeVariables
    , UndecidableInstances, FlexibleInstances #-}
module Data.TCache.IResource where

import Data.Typeable
import System.IO.Unsafe
import Control.Concurrent.STM
import Control.Concurrent
import Control.Exception as Exception
import System.IO
import System.IO.Error
import Data.List(elemIndices)
import Control.Monad(when,replicateM)
import Data.List(isInfixOf)


{- | Must be defined for every object to be cached.
-}
class IResource a where 
        {- The `keyResource string must be a unique  since this is used to index it in the hash table. 
        when accessing a resource, the user must provide a partial object for wich the key can be obtained.
        for example:
        
        @data Person= Person{name, surname:: String, account :: Int ....)
        
        keyResource Person n s ...= n++s@
        
        the data being accesed must define the fields used by keyResource. For example

         @  readResource Person {name="John", surname= "Adams"}@
        
        leaving the rest of the fields undefined

        when using default file persistence, the key is used as file name. so it must contain valid filename characters
        
        -}
        keyResource :: a -> String             -- ^ must be defined

        {- | Implements the database access and marshalling of the object.
        while the database access must be strict, the marshaling must be lazy if, as is often the case,
        some parts of the object are not really accesed.
        If the object contains DBRefs, this avoids unnecesary cache lookups.
        This method is called within 'atomically' blocks.
        Since STM transactions retry, readResourceByKey may be called twice in strange situations. So it must be idempotent, not only in the result but also in the effect in the database
        . However, because it is executed by 'safeIOToSTM' it is guaranteed that the execution is not interrupted.
        -}
        readResourceByKey :: String -> IO(Maybe a)
        readResourceByKey k= return . head =<< readResourcesByKey [k]
        -- | hopefully optimized read of many objects by key.
        readResourcesByKey :: [String] -> IO [Maybe a]
        readResourcesByKey = mapM readResourceByKey

        -- To allow accesses not by key but by any criteria based on the content of the record fields
        -- included in the -partial- definition of the input record. (it defaults as @readResourceByKey $ keyResource x@)
        readResource :: a -> IO (Maybe a)
        readResource x = readResourceByKey $ keyResource x

        -- | To write into persistent storage. It must be strict.  
        -- Since STM transactions may retry, @writeResource@ must be idempotent, not only in the result but also in the effect in the database.
        -- . However, because it is executed by 'safeIOToSTM' it is guaranteed that the execution is not interrupted.
        -- All the new obbects are writeen to the database on synchromization,
        -- so writeResource must not autocommit.
        -- Commit code must be located in the postcondition. (see  `setConditions`)
        -- Since there is no provision for rollback from failure in writing to
        -- persistent storage, 'writeResource' must retry until success.
    	writeResource:: a-> IO()
    	writeResource r= writeResources [r]
    	
    	-- | multiple write (hopefully) in a single request. That is up to you and your backend
    	-- . Defined by default as 'mapM_ writeResource'
    	writeResources :: [a] -> IO()
    	writeResources= mapM_ writeResource

        -- | Delete the resource. It is called syncronously. So it must commit   
    	delResource:: a-> IO()
    	delResource x= delResources [x]
    	
        delResources :: [a] -> IO()
        delResources= mapM_ delResource
-- | Resources data definition used by 'withSTMResources'    
data Resources a b
       = Retry             -- ^ forces a retry
       | Resources
          { toAdd :: [a]    -- ^ resources to be inserted back in the cache
          , toDelete :: [a] -- ^ resources to be deleted from the cache and from permanent storage
          , toReturn :: b   -- ^ result to be returned
          }


-- | Empty resources: @resources= Resources  [] [] ()@
resources :: Resources a ()
resources =  Resources  [] [] ()


{-

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
-}
class Serializable a where
  serialize   :: a -> String
  deserialize :: String -> a


-}



{-
defaultReadResource :: (Serializable a, Indexable a, Typeable a) =>  a -> IO (Maybe a)
defaultReadResource x= defaultReadResourceByKey $ key x


defaultReadResourceByKey :: (Serializable a, Indexable a) =>  String-> IO (Maybe a)
defaultReadResourceByKey k= iox
     where
     iox = handle handler $ do   
             s <-  readFileStrict  filename :: IO String 
             return $ Just (deserialize s )                                                         -- `debug` ("read "++ filename)
     
     filename=  defPathIO iox  ++ k

     defPathIO :: (Serializable a, Indexable a)=> IO (Maybe a) -> String
     defPathIO iox= defPath x
       where
       Just x= unsafePerformIO $ (return $ Just undefined)  `asTypeOf`  iox

 
     handler :: (Serializable a, Indexable a) =>   IOError ->  IO (Maybe a)
     handler  e
      | isAlreadyInUseError e = defaultReadResourceByKey  k                         
      | isDoesNotExistError e = return Nothing
      | otherwise= if ("invalid" `isInfixOf` ioeGetErrorString e)
         then
            error $ ( "readResource: " ++ show e) ++ " defPath and/or keyResource are not suitable for a file path"
              
         else defaultReadResourceByKey  k


defaultWriteResource :: (Serializable a, Indexable a) => a-> IO()
defaultWriteResource x= safeWrite filename (serialize x)   --  `debug` ("write "++filename)
  where
  filename= defPath x ++ key x

safeWrite filename str= handle  handler  $ writeFile filename str
     where          
     handler  (e :: IOError)
       | isDoesNotExistError e=do 
                  createDirectoryIfMissing True $ take (1+(last $ elemIndices '/' filename)) filename   --maybe the path does not exist
                  safeWrite filename str               

       | otherwise =do
                --phPutStrLn stderr $ "defaultWriteResource:  " ++ show e ++  " in file: " ++ filename ++ " retrying"
                safeWrite filename str
              
defaultDelResource :: (Indexable a) => a -> IO()
defaultDelResource x=  handle (handler filename) $ removeFile filename  --`debug` ("delete "++filename)
     where
     filename= defPath x ++ key x
     handler :: String -> IOError -> IO ()
     handler file e
       | isDoesNotExistError e= return ()
       | isAlreadyInUseError e= do
            --hPutStrLn stderr $ "defaultDelResource: busy"  ++  " in file: " ++ filename ++ " retrying"
            threadDelay 1000000
            defaultDelResource x   
       | otherwise = do
           --hPutStrLn stderr $ "defaultDelResource:  " ++ show e ++  " in file: " ++ filename ++ " retrying"
           threadDelay 1000000
           defaultDelResource x




-- | Strict read from file, needed for default file persistence
readFileStrict f = openFile f ReadMode >>= \ h -> readIt h `finally` hClose h
  where
  readIt h= do
      s   <- hFileSize h
      let n= fromIntegral s
      str <- replicateM n (hGetChar h) 
      return str
    

newtype Transient a= Transient a

-- | Transient wraps any indexable object for living in the cache, but not
-- in persistent storage. This is useful for memoization.
-- @Transient x@ is neither writeen nor read.
instance Indexable a => IResource (Transient a) where
   keyResource (Transient x)= key x
   readResourceByKey = const $ return Nothing
   writeResource= const $ return ()
   delResource = const $ return ()

-}

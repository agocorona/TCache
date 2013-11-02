{-# LANGUAGE   TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables, DeriveDataTypeable #-}

{- | some internal definitions. To use default persistence, import
@Data.TCache.DefaultPersistence@ instead -}

module Data.TCache.Defs  where
import Data.Typeable
import Control.Concurrent.STM(TVar)

import Data.TCache.IResource

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

import qualified Data.ByteString.Lazy.Char8 as B

--import Debug.Trace
--(!>) = flip trace

type AccessTime = Integer
type ModifTime  = Integer


data Status a= NotRead | DoNotExist | Exist a deriving Typeable

data Elem a= Elem !a !AccessTime !ModifTime   deriving Typeable

type TPVar a=   TVar (Status(Elem a))

data DBRef a= DBRef !String  !(TPVar a)  deriving Typeable



castErr a= r where
  r= case cast a of
      Nothing -> error $ "Type error: " ++ (show $ typeOf a) ++ " does not match "++ (show $ typeOf r)
                          ++ "\nThis means that objects of these two types have the same key \nor the retrieved object type is not the previously stored one for the same key\n"
      Just x  -> x


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
                                -- IMPORTANT:  defPath must depend on the datatype, not the value (must be constant). Default is ".tcachedata/"
    defPath =  const ".tcachedata/"

--instance IResource a => Indexable a where
--   key x= keyResource x


instance Indexable String where
  key= id

instance Indexable Int where
  key= show

instance Indexable Integer where
  key= show


instance Indexable () where
  key _= "void"


{- | Serialize is an alternative to the IResource class for defining persistence in TCache.
The deserialization must be as lazy as possible.
serialization/deserialization are not performance critical in TCache

Read, Show,  instances are implicit instances of Serializable

>    serialize  = show
>    deserialize= read

Since write and read to disk of to/from the cache are not be very frequent
The performance of serialization is not critical.
-}
class Serializable a  where
  serialize   :: a -> B.ByteString
  deserialize :: B.ByteString -> a
  deserialKey :: String -> B.ByteString -> a
  deserialKey _ v= deserialize v
  setPersist  :: a -> Maybe Persist              -- ^ `defaultPersist` if Nothing
  setPersist =  const Nothing

type Key= String
--instance (Show a, Read a)=> Serializable a where
--  serialize= show
--  deserialize= read


-- | a persist mechanism has to implement these three primitives
-- 'filePersist' is the default file persistence
data Persist = Persist{
       readByKey   ::  (Key -> IO(Maybe B.ByteString)) -- ^  read by key. It must be strict
     , write       ::  (Key -> B.ByteString -> IO())   -- ^  write. It must be strict
     , delete      ::  (Key -> IO())}                  -- ^  delete

-- | Implements default persistence of objects in files with their keys as filenames
filePersist   = Persist
    {readByKey= defaultReadByKey
    ,write    = defaultWrite
    ,delete   = defaultDelete}

defaultPersistIORef = unsafePerformIO $ newIORef  filePersist

-- | Set the default persistence mechanism of all 'serializable' objetcts. By default it is 'filePersist'
--
-- this statement must be the first one before any other TCache call
setDefaultPersist p= writeIORef defaultPersistIORef p

getDefaultPersist =  unsafePerformIO $ readIORef defaultPersistIORef

getPersist x= unsafePerformIO $ case setPersist x of
     Nothing -> readIORef defaultPersistIORef
     Just p  -> return p
  `Exception.catch` (\(e:: SomeException) -> error $ "setPersist must depend on the type, not the value of the parameter for: "
                                                         ++  show (typeOf x)
                                                         ++ "error was:" ++ show e)


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
            error $  "defaultReadByKey: " ++ show e ++ " defPath and/or keyResource are not suitable for a file path:\n"++ k++"\""
              
         else defaultReadByKey  k


defaultWrite :: String-> B.ByteString -> IO()
defaultWrite filename x= safeWrite filename  x
safeWrite filename str= handle  handler  $ B.writeFile filename str   -- !> ("write "++filename)
     where          
     handler e-- (e :: IOError)
       | isDoesNotExistError e=do 
                  createDirectoryIfMissing True $ take (1+(last $ elemIndices '/' filename)) filename   --maybe the path does not exist
                  safeWrite filename str               


       | otherwise= if ("invalid" `isInfixOf` ioeGetErrorString e)
             then
                error  $ "defaultWriteResource: " ++ show e ++ " defPath and/or keyResource are not suitable for a file path: "++ filename
             else do
                hPutStrLn stderr $ "defaultWriteResource:  " ++ show e ++  " in file: " ++ filename ++ " retrying"
                safeWrite filename str
              
defaultDelete :: String -> IO()
defaultDelete filename =do
     handle (handler filename) $ removeFile filename

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



defReadResourceByKey k= iox where
    iox= do
      let Persist f _ _ = getPersist  x
      f  file >>=  evaluate . fmap  (deserialKey k)
      where
      file= defPath x ++ k
      x= undefined `asTypeOf` (fromJust $ unsafePerformIO iox)

defWriteResource s= do
      let Persist _ f _ = getPersist  s
      f (defPath s ++ key s) $ serialize s

defDelResource s= do
      let Persist _ _ f = getPersist s
      f $ defPath s ++ key s


-- | Strict read from file, needed for default file persistence
readFileStrict f = openFile f ReadMode >>= \ h -> readIt h `finally` hClose h
  where
  readIt h= do
      s   <- hFileSize h
      let n= fromIntegral s
      str <- B.hGet h n
      return str


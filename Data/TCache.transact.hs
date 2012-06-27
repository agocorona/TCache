{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification, DeriveDataTypeable
    , FlexibleInstances, UndecidableInstances #-} 

{- | TCache is a transactional cache with configurable persistence that permits
STM transactions with objects thar syncronize sincromous or asyncronously with
their user defined storages. Default persistence in files is provided for testing purposes

 TCache implements ''DBRef' 's . They are persistent STM references  with a traditional 'readDBRef', 'writeDBRef' Haskell interface.
simitar to TVars ('newDBRef', 'readDBRef', 'writeDBRef' etc) but with added. persistence
. DBRefs are serializable, so they can be stored and retrieved. See some examples below
Because they are references,they point to other serializable registers.
This permits persistent mutable Inter-object relations

For simple transactions of lists of objects of the same type TCache implements
inversion of control primitives 'withSTMResources' and variants, that call pure user defined code for registers update.
See examples below.

Triggers in "Data.TCache.Triggers" are user defined hooks that are called back on register updates. That can be used for:


"Data.TCache.IndexQuery" implements an straighforwards pure haskell type safe query language  based
 on register field relations. This module must be imported separately.

"Data.TCache.IndexText" add full text search and content search to the qhery language

"Data.TCache.DefaultPersistence" define instances for key definition, serialization
and persistence, and default file persistence. The file persistence is now more reliable, and the embedded IO reads inside STM transactions are safe.


-}



 
module Data.TCache (
-- * Inherited from 'Control.Concurrent.STM'

 atomically
 ,STM
-- * synced atomical transaction
 ,atomicallySync
-- * Operations with cached database references
{-|  DBRefs are persistent cached database references in the STM monad
with read/write primitives, so the traditional syntax of Haskell STM references
can be used for  interfacing with databases. As expected, the DBRefs are transactional,
 because they operate in the STM monad.

DBRefs are references to  cached database objects. A DBRef is associated with its referred object and its key
Since DBRefs are serializable, they can be elements of mutable objects. They could point to other mutable objects
and so on, so DBRefs can act as "hardwired" relations from mutable objects
to other mutable objects in the database/cache. their referred objects are loaded, saved and flused
to and from the cache automatically depending on the cache handling policies and the access needs


DBRefs are univocally identified by its pointed object keys, so they can be compared, ordered and so on.
The creation of a DBRef, trough 'getDBRef' is pure. This permits an efficient lazy marshalling
of registers with references, such are indexes when are queried for some fields but not others.

Example: Car registers have references to Person regiters

@
data Person= Person {pname :: String} deriving  (Show, Read, Eq, Typeable)
data Car= Car{owner :: DBRef Person , cname:: String} deriving (Show, Read, Eq, Typeable)
@


Here the Car register point to the Person register trough the owner field

To permit persistence and being refered with DBRefs, define the Indexable instance
for these two register types:

@
instance Indexable Person where key Person{pname= n} = "Person " ++ n
instance Indexable Car where key Car{cname= n} = "Car " ++ n
@

Now we create a DBRef to a Person whose name is \"Bruce\"

>>> let bruce =   getDBRef . key $ Person "Bruce" :: DBRef Person

>>> show bruce
>"DBRef \"Person bruce\""

>>> atomically (readDBRef bruce)
>Nothing

'getDBRef' is pure and creates the reference, but not the referred object;
To create both the reference and the DBRef, use 'newDBRef'.
Lets create two Car's and its two Car DBRefs with bruce as owner:

>>> cars <- atomically $  mapM newDBRef [Car bruce "Bat Mobile", Car bruce "Porsche"]

>>> print cars
>[DBRef "Car Bat Mobile",DBRef "Car Porsche"]

>>> carRegs<- atomically $ mapM readDBRef cars
> [Just (Car {owner = DBRef "Person bruce", cname = "Bat Mobile"})
> ,Just (Car {owner = DBRef "Person bruce", cname = "Porsche"})]

try to write with 'writeDBRef'

>>> atomically . writeDBRef bruce $ Person "Other"
>*** Exception: writeDBRef: law of key conservation broken: old , new= Person bruce , Person Other

DBRef's can not be written with objects of different keys

>>> atomically . writeDBRef bruce $ Person "Bruce"

>>> let Just carReg1= head carRegs

now from the Car register it is possible to recover the owner's register

>>> atomically $ readDBRef ( owner carReg1)
>Just (Person {pname = "bruce"})



DBRefs, once the pointed cached object is looked up in the cache and found at creation, they does
not perform any further cache lookup afterwards, so reads and writes from/to DBRefs are faster
than *Resource(s) calls, which perform lookups everytime
in the cache

DBRef's and *Resource(s) primitives are completely interoperable. The latter operate implicitly with DBRef's

-}


,DBRef
,getDBRef
,keyObjDBRef
,newDBRef
--,newDBRefIO
,readDBRef
,writeDBRef
,delDBRef

-- * IResource class
{- | cached objects must be instances of IResource.
Such instances can be implicitly derived trough auxiliary clasess for file persistence
-}
,IResource(..)

-- * Operations with cached objects
{- | Operations with DBRef's can be performed implicitly with the \"traditional\" TCache operations
available in older versions.

In this example \"buy\" is a transaction where the user buy an item.
The spent amount is increased and the stock of the product is decreased:

@
data  Data=   User{uname:: String, uid:: String, spent:: Int} |
              Item{iname:: String, iid:: String, price:: Int, stock:: Int}
              deriving (Read, Show)

instance Indexable Data where
        key   User{uid=id}= id
        key   Item{iid=id}= id

user `buy` item=  'withResources'[user,item] buyIt
 where
    buyIt[Just us,Just it]
       | stock it > 0= [us',it']
       | otherwise   = error \"stock is empty for this product\"
      where
       us'= us{spent=spent us + price it}
       it'= it{stock= stock it-1}
    buyIt _ = error \"either the user or the item (or both) does not exist\"
@
-}
,resources      -- empty resources
,withSTMResources
,Resources(..)  -- data definition used to communicate object Inserts and Deletes to the cache
,withResources   
,withResource                           
,getResources     
,getResource     
,deleteResources 
,deleteResource

-- * Trigger operations
{- | Trriggers are called just before an object of the given type is created, modified or deleted.
The DBRef to the object and the new value is passed to the trigger.
The called trigger function has two parameters: the DBRef being accesed
(which still contains the old value), and the new value.
If the content of the DBRef is being deleted, the second parameter is 'Nothing'.
if the DBRef contains Nothing, then the object is being created

Example:

every time a car is added, or deleted, the owner's list is updated
this is done by the user defined trigger addCar

@
 addCar pcar (Just(Car powner _ )) = addToOwner powner pcar
 addCar pcar Nothing  = readDBRef pcar >>= \(Just car)-> deleteOwner (owner car) pcar

 addToOwner powner pcar=do
    Just owner <- readDBRef powner
    writeDBRef powner owner{cars= nub $ pcar : cars owner}

 deleteOwner powner pcar= do
   Just owner <- readDBRef powner
   writeDBRef powner owner{cars= delete  pcar $ cars owner}

 main= do
    'addTrigger' addCar
    putStrLn \"create bruce's register with no cars\"
    bruce \<- 'atomically' 'newDBRef' $ Person \"Bruce\" []
    putStrLn "add two car register with \"bruce\" as owner using the reference to the bruces register"
    let newcars= [Car bruce \"Bat Mobile\" , Car bruce \"Porsche\"]
    insert newcars
    Just bruceData \<- atomically $ 'readDBRef' bruce
    putStrLn "the trigger automatically updated the car references of the Bruce register"
    print . length $ cars bruceData
    print bruceData
@

gives:

> main
> 2
> Person {pname = "Bruce", cars = [DBRef "Car Porsche",DBRef "Car Bat Mobile"]}

-}

,addTrigger

-- * Cache control
,flushDBRef
,flushAll
,Cache       
,setCache        
,newCache        
,refcache        
,syncCache
,setConditions
,clearSyncCache
,numElems
,syncWrite
,SyncMode(..)
,defaultCheck
-- * other
,safeIOToSTM
)
where 


import GHC.Conc
import Control.Monad(when)
import Control.Monad.Trans 
import Data.HashTable as H
import Data.IORef
import System.IO.Unsafe
import System.IO(hPutStr, stderr)
import Data.Maybe
import Data.List
import Data.TCache.Defs
import Data.TCache.IResource
import Data.TCache.Triggers
import Control.Exception(handle,assert, bracket, SomeException)
import Data.Typeable
import System.Time
import System.Mem
import System.Mem.Weak 

import Control.Concurrent.MVar
import Control.Exception(catch, throw,evaluate)

import Debug.Trace
(!>) = flip trace

-- there are two references to the DBRef here
-- The Maybe one keeps it alive until the cache releases it for *Resources
-- calls which does not reference dbrefs explicitly
-- The weak reference keeps the dbref alive until is it not referenced elsewere
data CacheElem= forall a.(IResource a,Typeable a) => CacheElem (Maybe (DBRef a)) (Weak(DBRef a))

type Ht = HashTable String  CacheElem

data ToWrite= forall a.(Typeable a,IResource a ) => ToWrite (DBRef a)

instance Eq ToWrite where
   (ToWrite x)==(ToWrite y)= keyObjDBRef x == keyObjDBRef y
   
-- contains the hastable, last sync time and te list of references to be written in permanent storage
type Cache = IORef (Ht , Integer,[ToWrite])
data CheckTPVarFlags= AddToHash | NoAddToHash  


data SyncMode= Synchronous   -- ^ write state after every step
             | Asyncronous
                  {frecuency  :: Int                     -- ^ number of seconds between saves when asyncronous
                  ,check      :: (Integer-> Integer-> Integer-> Bool)  -- ^ The user-defined check-for-cleanup-from-cache for each object. 'defaultCheck' is an example
                  ,cacheSize  :: Int                     -- ^ size of the cache when async
                  }
             | SyncManual               -- ^ use Data.TCache.syncCache to write the state




tvSyncWrite= unsafePerformIO $ newIORef  (Synchronous, Nothing)

-- |
-- The execution log is cached in memory using the package `TCache`. This procedure defines the polcy for writing the cache into permanent storage.
--
-- For fast workflows, or when TCache` is used also for other purposes ,  `Asynchronous` is the best option
--
-- `Asynchronous` mode  invokes `clearSyncCache`. For more complex use of the syncronization
-- please use this `clearSyncCache`.
--
-- When interruptions are  controlled, use `SyncManual` mode and include a call to `syncCache` in the finalizaton code

syncWrite::  (Monad m, MonadIO m) => SyncMode -> m ()
syncWrite mode= do
     (_,thread) <- liftIO $ readIORef tvSyncWrite
     when (isJust thread ) $ liftIO . killThread . fromJust $ thread
     case mode of
          Synchronous -> modeWrite
          SyncManual  -> modeWrite
          Asyncronous time check maxsize -> do
               th <- liftIO  $ clearSyncCacheProc  time check maxsize >> return()
               liftIO $ writeIORef tvSyncWrite (mode,Just th)
     where
     modeWrite=
       liftIO  $ writeIORef tvSyncWrite (mode, Nothing)


class Monad m => HasTransactions m where 
    tatomically :: m a -> IO a
    tretry :: m a
    tadd ::  (Typeable a,IResource a) => DBRef a -> m ()
    tget :: m [ToWrite]
    transact :: m Bool
    tlift :: STM a -> m a

instance HasTransactions STM where
    tatomically = atomically
    tretry= retry
    tlift= id
    tadd x= unsafeIOToSTM . atomicModifyIORef refcache
           $ \(cache, t, xs) -> ((cache,t,  ToWrite x : xs),())
    tget = undefined safeIOToSTM $ atomicModifyIORef refcache 
           $ \(cache, t, xs) ->
              let t2= unsafePerformIO timeInteger
              in t2 `seq` ((cache,t2,[]),xs)
    transact= do 
       (savetype,_) <- unsafeIOToSTM $ readIORef tvSyncWrite
       case  savetype of
        Synchronous -> do
            sync 
            return True
        _ -> return True
       where
       sync= do
         l <-  tget
         tosave <- mapM (\(ToWrite x) ->readDBRef x >>= \my -> return $ fmap  Filtered my) $ nub l 
         unsafeIOToSTM . criticalSection saving . save  $ catMaybes tosave

          
-- | perform an atomic transaction and write to persistent storage if
--  the synchronization policies defined in 'syncWrite' is 'Synchronous'. 
-- Therefore, unlike 'atomic' it can perform synchronous transactions.
-- the synchronization process is definedd in the 'transact' method in the 'HasTransaction' instance.
-- The signature uses a generic monad instead of STM in order to allow future use of
-- more sophisticated mechanisms of synchronization such are distributed transactions
-- 
-- the STM monad uses  'syncCache'

atomicallySync :: HasTransactions m => m a -> IO a
atomicallySync proc= tatomically $ do
                        r <- proc
                        t <- transact
                        if t then return r else tretry


    
-- | set the cache. this is useful for hot loaded modules that will update an existing cache. Experimental
setCache :: Cache -> IO()
setCache ref = readIORef ref >>= \ch -> writeIORef refcache ch

-- the cache holder. stablished by default
refcache :: Cache  
refcache =unsafePerformIO $ newCache >>= newIORef

-- | newCache  creates a new cache. Experimental          
newCache  :: IO (Ht , Integer,[ToWrite])
newCache =do
        c <- H.new (==) hashString
        return (c,0,[])

-- | return the  total number of DBRefs in the cache. For debug purposes
-- This does not count the number of objects in the cache since many of the DBRef
-- may not have the pointed object loaded. Itś O(n).
numElems :: IO Int
numElems= do
   (cache, _,_) <- readIORef refcache
   elems <-   toList cache
   return $ length elems


deRefWeakSTM = unsafeIOToSTM . deRefWeak

deleteFromCache :: (IResource a, Typeable a) => DBRef a -> IO ()
deleteFromCache (DBRef k tv)=   do
    (cache, _,_) <- readIORef refcache
    H.delete cache k    -- !> ("delete " ++ k)


-- | return the reference value. If it is not in the cache, it is fetched
-- from the database.
readDBRef :: (IResource a, Typeable a)  => DBRef a -> STM (Maybe a)
readDBRef dbref@(DBRef key  tv)= do
  r <- readTVar tv
  case r of
   Exist (Elem x _ mt) -> do
       t <- unsafeIOToSTM timeInteger
       writeTVar tv  . Exist $ Elem x t mt
       return $ Just x
   DoNotExist -> return $ Nothing
   NotRead ->  do
     r <-   safeIOToSTM $ readResourceByKey key
     case r of
       Nothing -> writeTVar tv DoNotExist >> return Nothing
       Just x  -> do
           t <- unsafeIOToSTM timeInteger
           writeTVar tv $ Exist $ Elem  x t t
           return $ Just  x

-- | write in the reference a value
-- The new key must be the same than the old key of the previous object stored
-- otherwise, an error "law of key conservation broken" will be raised
--
-- WARNING: the value to be written in the DBRef must be fully evaluated. Delayed evaluations at
-- serialization time can cause inconsistencies in the database.
-- In future releases this will be enforced.
writeDBRef :: (IResource a, Typeable a)  => DBRef a -> a -> STM ()
writeDBRef dbref@(DBRef key  tv) x= x `seq` do
 let newkey= keyResource x
 if newkey /= key
   then  error $ "writeDBRef: law of key conservation broken: old , new= " ++ key ++ " , "++newkey
   else do
    tlift $ do
        applyTriggers  [dbref] [Just x]
        t <- unsafeIOToSTM timeInteger
        writeTVar tv $ Exist $ Elem x t t  --  !> ("writeDBRef "++ key)
    tadd dbref 
    return()




instance  Show (DBRef a) where
  show (DBRef  key _)=   "DBRef \""++ key ++ "\""

instance  (IResource a, Typeable a) => Read (DBRef a) where
    readsPrec n ('D':'B':'R':'e':'f':' ':'\"':str)=
       let (key,nstr) =  break (== '\"') str
       in  [( getDBRef key :: DBRef a, tail  nstr)]
    readsPrec _ _ = []

instance Eq (DBRef a) where
  DBRef k _ == DBRef k' _ =  k==k'

instance Ord (DBRef a) where
  compare (DBRef k _)  (DBRef k' _) = compare k k'

-- | return the key of the object pointed to by the DBRef
keyObjDBRef ::  DBRef a -> String
keyObjDBRef (DBRef k _)= k


-- | get the reference to the object in the cache. if it does not exist, the reference is created empty.
-- Every execution of 'getDBRef' returns the same unique reference to this key,
-- so it can be safely considered pure. This is a property useful because deserialization
-- of objects with unused embedded DBRef's  do not need to marshall them eagerly
--  Tbis also avoid unnecesary cache lookups of the pointed objects.
{-# NOINLINE getDBRef #-}
getDBRef :: (Typeable a, IResource a) => String -> DBRef a
getDBRef key=   unsafePerformIO $! getDBRef1 $! key where
 getDBRef1 :: (Typeable a, IResource a) =>  String -> IO (DBRef a)
 getDBRef1 key= do
  (cache,_,_) <-  readIORef refcache -- !> ("getDBRef "++ key)
  r <- H.lookup cache  key
  case r of
   Just (CacheElem  _ w) -> do
     mr <-  deRefWeak w
     case mr of
        Just dbref@(DBRef _  tv) -> return $! castErr dbref
        Nothing -> finalize w >>  getDBRef1 key  -- the weak pointer has not executed his finalizer

   Nothing -> do
     tv<- newTVarIO NotRead
     dbref <- evaluate $ DBRef key  tv
     w <- mkWeakPtr  dbref . Just $ deleteFromCache dbref
     H.update cache key (CacheElem Nothing w)
     return  dbref

{- - | Create the object passed as parameter (if it does not exist) and
-- return its reference in the IO monad.
-- If an object with the same key already exists, it is returned as is
-- If not, the reference is created with the new value.
-- If you like to update in any case, use 'getDBRef' and 'writeDBRef' combined  
newDBRefIO :: (IResource a,Typeable a) => a -> IO (DBRef a)
newDBRefIO x= do
 let key = keyResource x
 mdbref <- mDBRefIO key
 case mdbref of
   Right dbref -> return dbref

   Left cache -> do
     tv<- newTVarIO  DoNotExist
     let dbref= DBRef key  tv
     w <- mkWeakPtr  dbref . Just $ deleteFromCache dbref
     H.update cache key (CacheElem Nothing w)
     t <-  timeInteger
     atomically $ do
       applyTriggers [dbref] [Just x]      --`debug` ("before "++key)
       writeTVar tv  . Exist $ Elem x t t
       return dbref
       
-}
         

----  get a single DBRef if exist 
--mDBRefIO
--       :: (IResource a, Typeable a)
--       => String                       -- ^ the list of partial object definitions for which keyResource can be extracted
--       -> IO (Either Ht (DBRef a))     -- ^ ThTCache.hse TVars that contain such objects
--mDBRefIO k= do
--    (cache,_) <-  readIORef refcache
--    r <-   H.lookup cache  k
--    case r of
--     Just (CacheElem _ w) -> do
--        mr <-  deRefWeak w
--        case mr of
--          Just dbref ->  return . Right $! castErr dbref
--          Nothing ->  finalize w >> mDBRefIO k
--     Nothing -> return $ Left cache 



-- | Create the object passed as parameter (if it does not exist) and
-- return its reference in the STM monad.
-- If an object with the same key already exists, it is returned as is
-- If not, the reference is created with the new value.
-- If you like to update in any case, use 'getDBRef' and 'writeDBRef' combined
-- if you  need to create the reference and the reference content, use 'newDBRef'

newDBRef ::   (IResource a, Typeable a) => a -> STM  (DBRef a)  
newDBRef x = do
  let ref= getDBRef $! keyResource x

  mr <- readDBRef  ref
  case mr of
    Nothing -> writeDBRef ref x >> return ref -- !> " write"
    Just r -> return ref                      -- !> " non write"
    
--newDBRef ::   (IResource a, Typeable a) => a -> STM  (DBRef a)
--newDBRef x = do
--  let key= keyResource x
--  mdbref <-  unsafeIOToSTM $ mDBRefIO  key
--  case mdbref of     
--   Right dbref -> return dbref
--   Left cache -> do
--      t  <- unsafeIOToSTM timeInteger
--      tv <- newTVar DoNotExist
--      let dbref= DBRef key  tv
--      (cache,_) <- unsafeIOToSTM $ readIORef refcache
--      applyTriggers [dbref] [Just x]
--      writeTVar tv   . Exist $ Elem x t t
--      unsafeIOToSTM $ do
--        w <- mkWeakPtr dbref . Just $ deleteFromCache dbref  
--        H.update cache key ( CacheElem Nothing w)
--      return dbref

-- | delete the content of the DBRef form the cache and from permanent storage
delDBRef :: (IResource a, Typeable a) => DBRef a -> STM()
delDBRef dbref@(DBRef k tv)= do
  mr <- readDBRef dbref
  case mr of
   Just x -> do
     applyTriggers [dbref] [Nothing]
     writeTVar tv DoNotExist

     safeIOToSTM $ criticalSection saving  $ delResource x

   Nothing -> return ()

    		




-- | deletes the pointed object from the cache, not the database (see 'delDBRef')
-- useful for cache invalidation when the database is modified by other process
flushDBRef ::  (IResource a, Typeable a) =>DBRef a -> STM()
flushDBRef (DBRef _ tv)=   writeTVar  tv  NotRead


-- | drops the entire cache.
flushAll :: STM ()
flushAll = do
 (cache, time, _) <- unsafeIOToSTM $ readIORef refcache
 elms <- unsafeIOToSTM $ toList cache
 mapM_ (del cache) elms
 where
 del cache ( _ , CacheElem _ w)= do
      mr <- unsafeIOToSTM $ deRefWeak w
      case mr of
        Just (DBRef _  tv) ->  writeTVar tv NotRead
        Nothing -> unsafeIOToSTM (finalize w) 



-- | This is the main function for the *Resource(s) calls. All the rest derive from it. The results are kept in the STM monad
-- so it can be part of a larger STM transaction involving other DBRefs
-- The 'Resources' register  returned by the user-defined function  is interpreted as such:
--
--  * 'toAdd':  the content of this field will be added/updated to the cache
--
--  * 'toDelete': the content of this field will be removed from the cache and from permanent storage
--
--  * 'toReturn': the content of this field will be returned by 'withSTMResources'
--
-- WARNING: To catch evaluations errors at the right place, the values to be written must be fully evaluated.
-- .Errors in delayed evaluations at serialization time can cause inconsistencies in the database.

withSTMResources :: (IResource a, Typeable a)=> [a]        -- ^ the list of resources to be retrieved
                     -> ([Maybe a]-> Resources a x)   -- ^ The function that process the resources found and return a Resources structure
                     -> STM x                  -- ^ The return value in the STM monad.

withSTMResources rs f=  do
  (cache,_,_) <- unsafeIOToSTM $ readIORef refcache
  mtrs      <- takeDBRefs rs cache AddToHash
  
  mrs <- mapM mreadDBRef mtrs
  case f mrs of
      Retry  -> retry
      Resources  as ds r  -> do

          applyTriggers (map (getDBRef . keyResource) ds) (repeat (Nothing  `asTypeOf` (Just(head ds))))
          delListFromHash cache   ds
          releaseTPVars as cache

          safeIOToSTM $ bracket
             (takeMVar saving)
             (putMVar saving)
             $ const $ mapM_ delResource ds
          return r
  
  where
  mreadDBRef :: (IResource a, Typeable a) => Maybe (DBRef a) -> STM (Maybe a)
  mreadDBRef (Just dbref)= readDBRef dbref 
  mreadDBRef Nothing    =  return Nothing
 
 
-- | update of a single object in the cache
--
-- @withResource r f= 'withResources' [r] (\[mr]-> [f mr])@
withResource:: (IResource  a, Typeable a)
              => a                    -- ^ prototypes of the object to be retrieved for which keyResource can be derived
              -> (Maybe a-> a)      -- ^ update function that return another full object
              -> IO ()
withResource r f= withResources [r] (\[mr]-> [f mr])


-- |  to atomically add/modify many objects in the cache

-- @ withResources rs f=  atomically $ 'withSTMResources' rs f1 >> return() where   f1 mrs= let as= f mrs in  Resources  as [] ()@
withResources:: (IResource a,Typeable a)=> [a]-> ([Maybe a]-> [a])-> IO ()
withResources rs f=  atomically $ withSTMResources rs f1 >> return() where 
     f1 mrs= let as= f mrs in  Resources  as [] ()
         
-- | to read a resource from the cache.

-- @getResource r= do{mr<- 'getResources' [r];return $! head mr}@
getResource:: (IResource a, Typeable a)=>a-> IO (Maybe a)
getResource r= do{mr<- getResources [r];return $! head mr}

--- | to read a list of resources from the cache if they exist

--  | @getResources rs= atomically $ 'withSTMResources' rs f1 where  f1 mrs= Resources  [] [] mrs@
getResources:: (IResource a, Typeable a)=>[a]-> IO [Maybe a]
getResources rs= atomically $ withSTMResources rs f1 where
  f1 mrs= Resources  [] [] mrs
		

-- | delete the   resource from cache and from persistent storage.
-- @ deleteResource r= 'deleteResources' [r] @
deleteResource :: (IResource a, Typeable a) => a -> IO ()
deleteResource r= deleteResources [r]

-- | delete the list of resources from cache and from persistent storage.

-- @  deleteResources rs= atomically $ 'withSTMResources' rs f1 where  f1 mrs = Resources  [] (catMaybes mrs) ()@
deleteResources :: (IResource a, Typeable a) => [a] -> IO ()
deleteResources rs= atomically $ withSTMResources rs f1 where
   f1 mrs = resources {toDelete=catMaybes mrs}
   

takeDBRefs :: (IResource a, Typeable a) => [a] -> Ht  -> CheckTPVarFlags -> STM [Maybe (DBRef a)] 
takeDBRefs rs cache addToHash=  mapM (takeDBRef cache addToHash)  rs  


{-# NOINLINE takeDBRef #-}
takeDBRef :: (IResource a, Typeable a) =>  Ht  -> CheckTPVarFlags -> a -> STM(Maybe (DBRef a))
takeDBRef cache flags x =do
 
   let  keyr= keyResource x
   c <- unsafeIOToSTM $ H.lookup cache keyr
   case c of
       Just  (CacheElem _ w) -> do
          mr <- unsafeIOToSTM $ deRefWeak w
          case mr of
            Just dbref -> return . Just $! castErr dbref
            Nothing -> unsafeIOToSTM (finalize w)  >> takeDBRef cache flags x
       Nothing   -> do
           safeIOToSTM $ readToCache flags cache  keyr x -- unsafeIOToSTM $ readResourceByKey keyr                      

   where
   readToCache flags cache key x= do
       mr <- readResource x
       case mr of
            Nothing -> return Nothing
            Just r2 -> do
               ti  <-   timeInteger
               tvr <-   newTVarIO . Exist $ Elem r2 ti ti
               case flags of
                   NoAddToHash -> return . Just $ DBRef key  tvr
                   AddToHash   -> do
                      dbref <- evaluate $ DBRef key  tvr
                      w <- mkWeakPtr  dbref . Just $ deleteFromCache dbref 
                      H.update cache key (CacheElem (Just dbref) w)
                      return $ Just dbref
     -- !> ("readToCache "++ key)
      

                
timeInteger= do TOD t _ <- getClockTime
                return t                                    

        

	

releaseTPVars :: (IResource a,Typeable a)=> [a] -> Ht  -> STM ()
releaseTPVars rs cache = mapM_  (releaseTPVar cache)  rs
    
releaseTPVar :: (IResource a,Typeable a)=>  Ht -> a  -> STM ()
releaseTPVar cache  r =do
	c <- unsafeIOToSTM $ H.lookup cache keyr
	case c of
	    Just  (CacheElem    _ w) -> do
	        mr <-  unsafeIOToSTM $ deRefWeak w
	        case mr of
	            Nothing -> unsafeIOToSTM (finalize w) >> releaseTPVar cache  r		
	            Just dbref@(DBRef key  tv) -> do
	            applyTriggers [dbref] [Just (castErr r)]
	            tadd dbref
                    t <- unsafeIOToSTM  timeInteger
                    writeTVar tv . Exist  $ Elem  (castErr r)  t t	
				

	    Nothing   ->  do
	        ti  <- unsafeIOToSTM timeInteger
	        tvr <- newTVar NotRead
	        dbref <- unsafeIOToSTM . evaluate $ DBRef keyr  tvr
	        applyTriggers [dbref] [Just r]
	        tadd dbref
	        writeTVar tvr . Exist $ Elem r ti ti
	        w <- unsafeIOToSTM . mkWeakPtr dbref $ Just $ deleteFromCache dbref
	        unsafeIOToSTM $ H.update cache keyr (CacheElem (Just dbref) w)-- accesed and modified XXX
	        return ()				 
				
						
	where 	keyr= keyResource r
                

		         

delListFromHash :: IResource a => Ht -> [a] -> STM ()
delListFromHash  cache  xs= mapM_ del xs
 where
 del :: IResource a => a -> STM ()
 del x= do
   let key= keyResource x
   mr <- unsafeIOToSTM $ H.lookup cache key
   case mr of
     Nothing -> return ()
     Just (CacheElem _ w) -> do
      mr <- unsafeIOToSTM $ deRefWeak w
      case mr of
        Just dbref@(DBRef _  tv) -> do
           writeTVar tv DoNotExist
        Nothing -> do
          unsafeIOToSTM (finalize w) >> del  x

   

updateListToHash hash kv= mapM (update1 hash) kv where
	update1 h (k,v)= update h k v



-- | Start the thread that periodically call 'clearSyncCache' to clean and writes on the persistent storage. 
-- Otherwise, 'syncCache' must be invoked explicitly or no persistence will exist.
-- Cache writes allways save a coherent state
clearSyncCacheProc :: 
         Int                          -- ^ number of seconds betwen checks. objects not written to disk are written
      -> (Integer -> Integer-> Integer-> Bool)  -- ^ The user-defined check-for-cleanup-from-cache for each object. 'defaultCheck' is an example
      -> Int                          -- ^ The max number of objects in the cache, if more, the  cleanup starts
      -> IO ThreadId           -- ^ Identifier of the thread created
clearSyncCacheProc  time check sizeObjects= forkIO  clear   
 where
 clear =handle ( \ (e :: SomeException)-> hPutStr stderr (show e) >> clear ) $ do
    	threadDelay (fromIntegral$ time * 1000000)
    	clearSyncCache   check sizeObjects   --`debug` "CLEAR"
    	clear

data Filtered= forall a. IResource a => Filtered a
 
-- | Force the atomic write of all cached objects modified since the last save into permanent storage
-- Cache writes allways save a coherent state

criticalSection mv f= bracket
  (takeMVar mv)
  (putMVar mv)
  $ const $ f

criticalSectionSTM mt f= do
  r <- readTVar mt
  if  r == False then retry
   else do
    writeTVar mt True
    f
    writeTVar mt False

syncCache ::  IO () 
--syncCache  = bracket
--  (takeMVar saving)
--  (putMVar saving)
--  $ const $ do
syncCache= criticalSection saving $ do
      tosave <- atomically $ do
                   l <-  tget   !> "syncCache"
                   mapM (\(ToWrite x) ->readDBRef x >>= \my -> return $ fmap  Filtered my) $ nub l 
      save  $ catMaybes tosave
 
  



-- |Saves the unsaved elems of the cache.
--  Allways save a coherent state
--  It deletes some elems of  the cache when the number of elems > 'maxObjects'.
--  The deletion depends on the checking policy. 'defaultCheck' is the one implemented
clearSyncCache ::  (Integer -> Integer-> Integer-> Bool)-> Int -> IO ()
clearSyncCache check maxObjects= criticalSection saving $ do
      t <- timeInteger

      tosave <- atomically $ do
                   l <- tget
                   mapM (\(ToWrite x) ->readDBRef x >>= \my -> return $ fmap  Filtered my) $ nub l 
      save  $ catMaybes tosave

      (cache,lastSync,_) <- readIORef refcache
      elems <- toList cache
      (elems, size) <- atomically $ extract elems lastSync

      when (size > maxObjects) $  forkIO (filtercache t cache lastSync elems)  >> return ()
         
  where

  -- delete elems from the cache according with the checking policy
  filtercache t cache lastSync elems= mapM_ filter elems
    where	    
    filter (CacheElem Nothing w)= return()  --alive because the dbref is being referenced elsewere
    filter (CacheElem (Just (DBRef key _)) w) = do
     mr <-  deRefWeak w
     case mr of
       Nothing ->    finalize w
       Just (DBRef _  tv) -> atomically $ do
         r <- readTVar tv
         case r of
    		Exist (Elem x lastAccess _ ) -> 
            		 if check t lastAccess lastSync
            		      then do
                              unsafeIOToSTM . H.update cache key $ CacheElem Nothing w
                              writeTVar tv NotRead
            		      else return ()
    		_    ->  return()

       

-- | ths is a default cache clearance check. It forces to drop from the cache all the
-- elems not accesed since half the time between now and the last sync
-- if it returns True, the object will be discarded from the cache
-- it is invoked when the cache size exceeds the number of objects configured
-- in 'clearSyncCacheProc' or 'clearSyncCache'
defaultCheck
       :: Integer    -- ^ current time in seconds
       -> Integer    -- ^ last access time for a given object
       -> Integer    -- ^ last cache syncronization (with the persisten storage)
       -> Bool       -- ^ return true for all the elems not accesed since half the time between now and the last sync
defaultCheck  now lastAccess lastSync
	| lastAccess > halftime = False
	| otherwise  = True

    where 
    halftime= now- (now-lastSync) `div` 2

refConditions= unsafePerformIO $ newIORef (return(), return())

setConditions :: IO() -> IO() -> IO()
-- ^ stablishes the procedures to call before and after saving with 'syncCache', 'clearSyncCache' or 'clearSyncCacheProc'. The postcondition of
-- database persistence should be a commit.
setConditions pre post= writeIORef refConditions (pre, post)

saving= unsafePerformIO $ newMVar False


save  tosave = do
     (pre, post) <-  readIORef refConditions 
     pre   -- !> "save"
     mapM (\(Filtered x)-> writeResource x )  tosave  
     post
  




-- extract the elements that can be potentially deleted safely
-- these that are not pointed directly by a DBRef which is part of an alive data structure
extract elems lastSave= filter1 [] (0:: Int)  elems
 where
  filter1  val n []= return (val, n)
  filter1  val n ((_, ch@(CacheElem mybe w)):rest)= do 
      mr <- unsafeIOToSTM $ deRefWeak w
      case mr of
        Nothing -> unsafeIOToSTM (finalize w) >> filter1 val n rest
        Just (DBRef key  tvr)  ->
         let  tofilter = case mybe of
                    Just _ -> ch:val
                    Nothing -> val
         in filter1  tofilter (n+1) rest


-- | executes the IO action in a separate thread
-- 
safeIOToSTM :: IO a -> STM a
safeIOToSTM req= unsafeIOToSTM  $ do
  tv   <- newEmptyMVar
  forkIO $ (req >>= putMVar  tv . Right)
          `Control.Exception.catch`
          (\(e :: SomeException) -> putMVar tv (Left e))
  r <- takeMVar tv
  case r of
   Right x -> return x
   Left e -> throw e





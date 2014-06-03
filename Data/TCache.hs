{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification, DeriveDataTypeable
    , FlexibleInstances, UndecidableInstances #-}

{- | TCache is a transactional cache with configurable persitence that permits
STM transactions with objects that syncronize sincromous or asyncronously with
their user defined storages. Default persistence in files is provided by default

 TCache implements ''DBRef' 's . They are persistent STM references  with a typical Haskell interface.
simitar to TVars ('newDBRef', 'readDBRef', 'writeDBRef' etc) but with added. persistence
. DBRefs are serializable, so they can be stored and retrieved.
Because they are references,they point to other serializable registers.
This permits persistent mutable Inter-object relations

For simple transactions of lists of objects of the same type TCache implements
inversion of control primitives 'withSTMResources' and variants, that call pure user defined code for registers update. Examples below.

Triggers in "Data.TCache.Triggers" are user defined hooks that are called back on register updates.
.They are used internally for indexing.

"Data.TCache.IndexQuery" implements an straighforwards pure haskell type safe query language  based
 on register field relations. This module must be imported separately.

"Data.TCache.IndexText" add full text search and content search to the query language

"Data.TCache.DefaultPersistence" has instances for key indexation , serialization
 and default file persistence. The file persistence is more reliable, and the embedded IO reads inside STM transactions are safe.

"Data.Persistent.Collection" implements a persistent, transactional collection with Queue interface as well as
 indexed access by key

-}




module Data.TCache (
-- * Inherited from 'Control.Concurrent.STM' and variations

 atomically
 ,atomicallySync
 ,STM
 ,unsafeIOToSTM
 ,safeIOToSTM

-- * Operations with cached database references
{-|  @DBRefs@ are persistent cached database references in the STM monad
with read/write primitives, so the traditional syntax of Haskell STM references
can be used for  interfacing with databases. As expected, the DBRefs are transactional,
 because they operate in the STM monad.

A @DBRef@ is associated with its referred object trough its key.
Since DBRefs are serializable, they can be elements of mutable cached objects themselves. They could point to other mutable objects
and so on, so DBRefs can act as \"hardwired\" relations from mutable objects
to other mutable objects in the database/cache. their referred objects are loaded, saved and flused
to and from the cache automatically depending on the cache handling policies and the access needs


@DBRefs@ are univocally identified by its pointed object keys, so they can be compared, ordered checked for equality so on.
The creation of a DBRef, trough 'getDBRef' is pure. This permits an efficient lazy access to the
 registers trouth their DBRefs by lazy marshalling of the register content on demand.

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
than *Resource(s) calls, which perform  cache lookups everytime the object is accessed

DBRef's and @*Resource(s)@ primitives are completely interoperable. The latter operate implicitly with DBRef's

-}


,DBRef
,getDBRef
,keyObjDBRef
,newDBRef
--,newDBRefIO
,readDBRef
,readDBRefs
,writeDBRef
,delDBRef

-- * @IResource@ class
{- | cached objects must be instances of IResource.
Such instances can be implicitly derived trough auxiliary clasess for file persistence
-}
,IResource(..)

-- * Operations with cached objects
{- | implement inversion of control primitives where  the user defines the objects to retrive. The primitives
then call a the defined function that, determines how to transform the objects retrieved,wich are sent
back to the storage and a result is returned.

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
,Resources(..)  -- data definition used to communicate object Inserts and Deletes to the cache
,resources      -- empty resources
,withSTMResources
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

Every time a car is added, or deleted, the owner's list is updated.
This is done by the user defined trigger addCar

@
 addCar pcar (Just(Car powner _ )) = addToOwner powner pcar
 addCar pcar Nothing  = readDBRef pcar >>= \\(Just car)-> deleteOwner (owner car) pcar

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
    putStrLn \"add two car register with \\"bruce\\" as owner using the reference to the bruces register\"
    let newcars= [Car bruce \"Bat Mobile\" , Car bruce \"Porsche\"]
    insert newcars
    Just bruceData \<- atomically $ 'readDBRef' bruce
    putStrLn \"the trigger automatically updated the car references of the Bruce register\"
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
{-- |

The mechanism for dropping elements from the cache is too lazy. `flushDBRef`, for example
just delete the data element  from the TVar, but the TVar node
remains attached to the table so there is no decrement on the number of elements.
The element is garbage collected unless you have a direct reference to the element, not the DBRef
Note that you can still have a valid reference to this element, but this element  is no longer
in the cache. The usual thing is that you do not have it, and the element will be garbage
collected (but still there will be a NotRead entry for this key!!!). If the DBRef is read again, the
TCache will go to permanent storage to retrieve it.

clear opertions such `clearsyncCache` does something similar:  it does not delete the
element from the cache. It just inform the garbage collector that there is no longer necessary to maintain
the element in the cache. So if the element has no other references (maybe you keep a
variable that point to that DBRef) it will be GCollected.
If this is not possible, it will remain in the cache and will be treated as such,
until the DBRef is no longer referenced by the program. This is done by means of a weak pointer

All these complications are necessary because the programmer can  handle DBRefs directly,
so the cache has no complete control of the DBRef life cycle, short to speak.

a DBRef can be in the states:

- `Exist`:  it is in the cache

- `DoesNotExist`: neither is in the cache neither in storage: it is like a cached "notfound" to
speed up repeated failed requests

- `NotRead`:  may exist or not in permanent storage, but not in the cache


In terms of Garbage collection it may be:



1 - pending garbage collection:  attached to the hashtable by means of a weak pointer: delete it asap

2 - cached: attached by a direct pointer and a weak pointer: It is being cached


clearsyncCache just pass elements from 2 to 1

--}
,flushDBRef
,flushKey
,invalidateKey
,flushAll
,Cache
,setCache
,newCache
--,refcache
,syncCache
,setConditions
,clearSyncCache
,numElems
,syncWrite
,SyncMode(..)
,clearSyncCacheProc
,defaultCheck
-- * Other
,onNothing
)
where


import GHC.Conc
import Control.Monad(when)
import qualified Data.HashTable.IO as H
import Data.IORef
import System.IO.Unsafe
import System.IO(hPutStr, stderr)
import Data.Maybe
import Data.Char(isSpace)
import Data.TCache.Defs
import Data.TCache.IResource
import Data.TCache.Triggers
import Control.Exception
import Data.Typeable
import System.Time
import System.Mem
import System.Mem.Weak

import Control.Concurrent.MVar
import Control.Exception(catch, throw,evaluate)

--import Debug.Trace
--(!>) = flip trace

-- there are two references to the DBRef here
-- The Maybe one keeps it alive until the cache releases it for *Resources
-- calls which does not reference dbrefs explicitly
-- The weak reference keeps the dbref alive until is it not referenced elsewere
data CacheElem= forall a.(IResource a,Typeable a) => CacheElem (Maybe (DBRef a)) (Weak(DBRef a))

type Ht = H.BasicHashTable   String  CacheElem

-- contains the hastable, last sync time
type Cache = IORef (Ht , Integer)
data CheckTPVarFlags= AddToHash | NoAddToHash

-- | Set the cache. this is useful for hot loaded modules that will update an existing cache. Experimental
setCache :: Cache -> IO()
setCache ref = readIORef ref >>= \ch -> writeIORef refcache ch

-- | The cache holder. stablished by default
refcache :: Cache
refcache =unsafePerformIO $ newCache >>= newIORef

-- |   Creates a new cache. Experimental
newCache  :: IO (Ht , Integer)
newCache =do
        c <- H.new  -- (==) H.hashString
        return (c,0)

-- | Return the  total number of DBRefs in the cache. For debug purposes.
-- This does not count the number of objects in the cache since many of the DBRef
-- may not have the pointed object loaded. It's O(n).
numElems :: IO Int
numElems= do
   (cache, _) <- readIORef refcache
   elems <-   H.toList cache
   return $ length elems


deRefWeakSTM = unsafeIOToSTM . deRefWeak

--deleteFromCache :: (IResource a, Typeable a) => DBRef a -> IO ()
--deleteFromCache (DBRef k tv)=   do
--    (cache, _) <- readIORef refcache
--    H.delete cache k    -- !> ("delete " ++ k)

fixToCache :: (IResource a, Typeable a) => DBRef a -> IO ()
fixToCache dbref@(DBRef k tv)= do
       (cache, _) <- readIORef refcache
       w <- mkWeakPtr dbref  $ Just $ fixToCache dbref
       H.insert cache k (CacheElem (Just dbref) w)
       return()

-- | Return the reference value. If it is not in the cache, it is fetched
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
       r <- safeIOToSTM $ readResourceByKey key
       case r of
         Nothing -> writeTVar tv DoNotExist >> return Nothing
         Just x  -> do
           t <- unsafeIOToSTM timeInteger
           writeTVar tv $ Exist $ Elem  x t (-1)
           return $ Just  x

-- | Read multiple DBRefs in a single request using the new 'readResourcesByKey'
readDBRefs :: (IResource a, Typeable a)  => [DBRef a] -> STM [(Maybe a)]
readDBRefs dbrefs= do
  let mf (DBRef key  tv)= do
      r <- readTVar tv
      case r of
        Exist (Elem x _ mt) -> do
          t <- unsafeIOToSTM timeInteger
          writeTVar tv  . Exist $ Elem x t mt
          return $ Right $ Just x
        DoNotExist -> return $ Right Nothing
        NotRead ->  return $ Left key
  inCache <- mapM mf dbrefs
  let pairs = foldr(\pair@(x,dbr) xs -> case x of Left k -> pair:xs; _ -> xs ) [] $ zip inCache dbrefs
  let (toReadKeys, dbrs) = unzip pairs
  let fromLeft (Left k)= k
      formLeft _ = error "this will never happen"
  rs <- safeIOToSTM . readResourcesByKey $ map fromLeft toReadKeys
  let processTVar (r, DBRef key  tv)= do
           case r of
             Nothing -> writeTVar tv DoNotExist
             Just x  -> do
               t <- unsafeIOToSTM timeInteger
               writeTVar tv $ Exist $ Elem  x t (-1)

  mapM_ processTVar $ zip rs dbrs
  let mix (Right x:xs) ys   = x:mix xs ys
      mix (Left _:xs) (y:ys)= y:mix xs ys

  return $ mix inCache rs

-- | Write in the reference a value
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
    applyTriggers  [dbref] [Just x]
    t <- unsafeIOToSTM timeInteger

    writeTVar tv $! Exist $! Elem x t t
    return()


instance  Show (DBRef a) where
  show (DBRef  key _)=   "DBRef \""++ key ++ "\""

instance  (IResource a, Typeable a) => Read (DBRef a) where
    readsPrec n str1= readit str
       where
       str = dropWhile isSpace str1
       readit ('D':'B':'R':'e':'f':' ':'\"':str1)=
         let   (key,nstr) =  break (== '\"') str1
         in  [( getDBRef key :: DBRef a, tail  nstr)]
       readit  _ = []

instance Eq (DBRef a) where
  DBRef k _ == DBRef k' _ =  k==k'

instance Ord (DBRef a) where
  compare (DBRef k _)  (DBRef k' _) = compare k k'

-- | Return the key of the object pointed to by the DBRef
keyObjDBRef ::  DBRef a -> String
keyObjDBRef (DBRef k _)= k


-- | Get the reference to the object in the cache. if it does not exist, the reference is created empty.
-- Every execution of 'getDBRef' returns the same unique reference to this key,
-- so it can be safely considered pure. This is a property useful because deserialization
-- of objects with unused embedded DBRef's  do not need to marshall them eagerly.
--  Tbis also avoid unnecesary cache lookups of the pointed objects.
{-# NOINLINE getDBRef #-}
getDBRef :: (Typeable a, IResource a) => String -> DBRef a
getDBRef key=   unsafePerformIO $! getDBRef1 $! key where
 getDBRef1 :: (Typeable a, IResource a) =>  String -> IO (DBRef a)
 getDBRef1 key = do
  (cache,_) <-  readIORef refcache   -- !> ("getDBRef "++ key)
  takeMVar getRefFlag
  r <- H.lookup cache  key
  case r of
   Just (CacheElem  mdb w) -> do
     putMVar getRefFlag ()
     mr <-  deRefWeak w
     case mr of
        Just dbref@(DBRef _ tv) ->
                case mdb of
                  Nothing -> return $! castErr dbref     -- !> "just"
                  Just _  -> do
                        H.insert cache key (CacheElem Nothing w) --to notify when the DBREf leave its reference
                        return $! castErr dbref
        Nothing -> finalize w >>  getDBRef1 key          -- !> "finalize"  -- the weak pointer has not executed his finalizer

   Nothing -> do
     tv <- newTVarIO NotRead                              -- !> "Nothing"
     dbref <- evaluate $ DBRef key  tv
     w <- mkWeakPtr  dbref . Just $ fixToCache dbref
     H.insert cache key (CacheElem Nothing w)
     putMVar getRefFlag ()
     return  dbref

getRefFlag= unsafePerformIO $ newMVar ()

{- | Create the object passed as parameter (if it does not exist) and
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
     w <- mkWeakPtr  dbref . Just $ fixToCache dbref
     H.insert cache key (CacheElem Nothing w)
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
{-# NOINLINE newDBRef #-}
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
--        w <- mkWeakPtr dbref . Just $ fixToCache dbref
--        H.insert cache key ( CacheElem Nothing w)
--      return dbref

-- | Delete the content of the DBRef form the cache and from permanent storage
delDBRef :: (IResource a, Typeable a) => DBRef a -> STM()
delDBRef dbref@(DBRef k tv)= do
  mr <- readDBRef dbref
  case mr of
   Just x -> do
     applyTriggers [dbref] [Nothing]
     writeTVar tv DoNotExist

     safeIOToSTM . criticalSection saving $ delResource x

   Nothing -> return ()



-- | Handles Nothing cases in a simpler way than runMaybeT.
-- it is used in infix notation. for example:
--
-- @result <- readDBRef ref \`onNothing\` error (\"Not found \"++ keyObjDBRef ref)@
--
-- or
--
-- @result <- readDBRef ref \`onNothing\` return someDefaultValue@
onNothing io onerr= do
  my <-  io
  case my of
   Just y -> return y
   Nothing -> onerr

-- | Deletes the pointed object from the cache, not the database (see 'delDBRef')
-- useful for cache invalidation when the database is modified by other process
flushDBRef ::  (IResource a, Typeable a) =>DBRef a -> STM()
flushDBRef (DBRef _ tv)=   writeTVar  tv  NotRead

-- | flush the element with the given key
flushKey key=  do
   (cache,time) <- unsafeIOToSTM $ readIORef refcache
   c <- unsafeIOToSTM $ H.lookup cache key
   case c of
       Just  (CacheElem _ w) -> do
          mr <- unsafeIOToSTM $ deRefWeak w
          case mr of
            Just (DBRef k tv) -> writeTVar  tv  NotRead
            Nothing -> unsafeIOToSTM (finalize w)  >> flushKey key
       Nothing   -> return ()

-- | label the object as not existent in database
invalidateKey key=  do
   (cache,time) <- unsafeIOToSTM $ readIORef refcache
   c <- unsafeIOToSTM $ H.lookup cache key
   case c of
       Just  (CacheElem _ w) -> do
          mr <- unsafeIOToSTM $ deRefWeak w
          case mr of
            Just (DBRef k tv) -> writeTVar  tv  DoNotExist
            Nothing -> unsafeIOToSTM (finalize w)  >> flushKey key
       Nothing   -> return ()


-- | drops the entire cache.
flushAll :: STM ()
flushAll = do
 (cache,time) <- unsafeIOToSTM $ readIORef refcache
 elms <- unsafeIOToSTM $ H.toList cache
 mapM_ (del cache) elms
 where
 del cache ( _ , CacheElem _ w)= do
      mr <- unsafeIOToSTM $ deRefWeak w
      case mr of
        Just (DBRef _  tv) ->  writeTVar tv NotRead
        Nothing -> unsafeIOToSTM (finalize w)



-- | This is the main function for the *Resource(s) calls. All the rest derive from it. The results are kept in the STM monad
-- so it can be part of a larger STM transaction involving other DBRefs.
-- The 'Resources' register  returned by the user-defined function  is interpreted as such:
--
--  * 'toAdd':  the content of this field will be added/updated to the cache
--
--  * 'toDelete': the content of this field will be removed from the cache and from permanent storage
--
--  * 'toReturn': the content of this field will be returned by 'withSTMResources'
--
-- WARNING: To catch evaluations errors at the right place, the values to be written must be fully evaluated.
-- Errors in delayed evaluations at serialization time can cause inconsistencies in the database.

withSTMResources :: (IResource a, Typeable a)=> [a]   -- ^ the list of resources to be retrieved
                     -> ([Maybe a]-> Resources a x)   -- ^ The function that process the resources found and return a Resources structure
                     -> STM x                  -- ^ The return value in the STM monad.

withSTMResources rs f=  do
  (cache,_) <- unsafeIOToSTM $ readIORef refcache
  mtrs      <- takeDBRefs rs cache AddToHash

  mrs <- mapM mreadDBRef mtrs
  case f mrs of
      Retry  -> retry
      Resources  as ds r  -> do
          applyTriggers (map (getDBRef . keyResource) ds) (repeat (Nothing  `asTypeOf` (Just(head ds))))
          delListFromHash cache   ds
          releaseTPVars as cache

          safeIOToSTM . criticalSection saving $ mapM_ delResource ds
          return r

  where
  mreadDBRef :: (IResource a, Typeable a) => Maybe (DBRef a) -> STM (Maybe a)
  mreadDBRef (Just dbref)= readDBRef dbref
  mreadDBRef Nothing    =  return Nothing


-- | Update of a single object in the cache
--
-- @withResource r f= 'withResources' [r] (\[mr]-> [f mr])@
{-# INLINE withResource #-}
withResource:: (IResource  a, Typeable a)   => a  -> (Maybe a-> a)  -> IO ()
withResource r f= withResources [r] (\[mr]-> [f mr])


-- |  To atomically add/modify many objects in the cache
--
-- @ withResources rs f=  atomically $ 'withSTMResources' rs f1 >> return() where   f1 mrs= let as= f mrs in  Resources  as [] ()@
{-# INLINE withResources #-}
withResources:: (IResource a,Typeable a)=> [a]-> ([Maybe a]-> [a])-> IO ()
withResources rs f=  atomically $ withSTMResources rs f1 >> return() where
     f1 mrs= let as= f mrs in  Resources  as [] ()

-- | To read a resource from the cache.
--
-- @getResource r= do{mr<- 'getResources' [r];return $! head mr}@
{-# INLINE getResource #-}
getResource:: (IResource a, Typeable a)=>a-> IO (Maybe a)
getResource r= do{mr<- getResources [r];return $! head mr}

-- | To read a list of resources from the cache if they exist
--
--  | @getResources rs= atomically $ 'withSTMResources' rs f1 where  f1 mrs= Resources  [] [] mrs@
{-# INLINE getResources #-}
getResources:: (IResource a, Typeable a)=>[a]-> IO [Maybe a]
getResources rs= atomically $ withSTMResources rs f1 where
  f1 mrs= Resources  [] [] mrs
		

-- | Delete the   resource from cache and from persistent storage.
--
-- @ deleteResource r= 'deleteResources' [r] @
{-# INLINE deleteResource #-}
deleteResource :: (IResource a, Typeable a) => a -> IO ()
deleteResource r= deleteResources [r]

-- | Delete the list of resources from cache and from persistent storage.
--
-- @  deleteResources rs= atomically $ 'withSTMResources' rs f1 where  f1 mrs = Resources  [] (catMaybes mrs) ()@
{-# INLINE deleteResources #-}
deleteResources :: (IResource a, Typeable a) => [a] -> IO ()
deleteResources rs= atomically $ withSTMResources rs f1 where
   f1 mrs = resources {toDelete=catMaybes mrs}

{-# INLINE takeDBRefs #-}
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
           safeIOToSTM $ readToCache flags cache  keyr
              -- unsafeIOToSTM $ readResourceByKey keyr

   where
   readToCache flags cache key= do
       mr <- readResource x
       case mr of
            Nothing -> return Nothing
            Just r2 -> do
               ti  <-   timeInteger
               tvr <-   newTVarIO . Exist $ Elem r2 ti (-1)
               case flags of
                   NoAddToHash -> return . Just $ DBRef key  tvr
                   AddToHash   -> do
                      dbref <- evaluate $ DBRef key  tvr
                      w <- mkWeakPtr  dbref . Just $ fixToCache dbref
                      H.insert cache key (CacheElem (Just dbref) w)
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
                    t <- unsafeIOToSTM  timeInteger
                    writeTVar tv . Exist  $ Elem  (castErr r)  t t	
				

	    Nothing   ->  do
	        ti  <- unsafeIOToSTM timeInteger
	        tvr <- newTVar NotRead
	        dbref <- unsafeIOToSTM . evaluate $ DBRef keyr  tvr
	        applyTriggers [dbref] [Just r]
	        writeTVar tvr . Exist $ Elem r ti ti
	        w <- unsafeIOToSTM . mkWeakPtr dbref $ Just $ fixToCache dbref
	        unsafeIOToSTM $ H.insert cache keyr (CacheElem (Just dbref) w)-- accesed and modified XXX
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
	update1 h (k,v)= H.insert h k v



-- | Start the thread that periodically call `clearSyncCache` to clean and writes on the persistent storage.
-- it is indirecly set by means of `syncWrite`, since it is more higuer level. I recommend to use the latter
-- Otherwise, 'syncCache' or `clearSyncCache` or `atomicallySync` must be invoked explicitly or no persistence will exist.
-- Cache writes allways save a coherent state
clearSyncCacheProc ::
         Int                          -- ^ number of seconds betwen checks. objects not written to disk are written
      -> (Integer -> Integer-> Integer-> Bool)  -- ^ The user-defined check-for-cleanup-from-cache for each object. 'defaultCheck' is an example
      -> Int                          -- ^ The max number of objects in the cache, if more, the  cleanup starts
      -> IO ThreadId           -- ^ Identifier of the thread created
clearSyncCacheProc  time check sizeObjects= forkIO  clear
 where
 clear = do
     threadDelay $ time * 1000000
     handle ( \ (e :: SomeException)-> hPutStr stderr (show e) >> clear ) $ do
    	clearSyncCache   check sizeObjects                                        -- !>  "CLEAR"
    	clear

criticalSection mv f= bracket
  (takeMVar mv)
  (putMVar mv)
  $ const $ f

-- | Force the atomic write of all cached objects modified since the last save into permanent storage.
-- Cache writes allways save a coherent state. As allways, only the modified objects are written.
syncCache ::  IO ()
syncCache  = criticalSection saving $ do
      (cache,lastSync) <- readIORef refcache  --`debug` "syncCache"
      t2<- timeInteger
      elems <- H.toList cache
      (tosave,_,_) <- atomically $ extract elems lastSync
      save tosave
      writeIORef refcache (cache, t2)


data SyncMode= Synchronous   -- ^ sync state to permanent storage when `atomicallySync` is invoked
             | Asyncronous   
                  {frecuency  :: Int                     -- ^ number of seconds between saves when asyncronous
                  ,check      :: (Integer-> Integer-> Integer-> Bool)  -- ^ The user-defined check-for-cleanup-from-cache for each object. 'defaultCheck' is an example
                  ,cacheSize  :: Int                     -- ^ size of the cache when async
                  }
             | SyncManual               -- ^ use `syncCache` to write the state




tvSyncWrite= unsafePerformIO $ newIORef  (Synchronous, Nothing)

-- | Specify the cache synchronization policy with permanent storage. See `SyncMode` for details
syncWrite::  SyncMode -> IO()
syncWrite mode= do
     (_,thread) <- readIORef tvSyncWrite
     when (isJust thread ) $ killThread . fromJust $ thread
     case mode of
          Synchronous -> modeWrite
          SyncManual  -> modeWrite
          Asyncronous time check maxsize -> do
               th <- clearSyncCacheProc  time check maxsize >> return()
               writeIORef tvSyncWrite (mode,Just th)
     where
     modeWrite= writeIORef tvSyncWrite (mode, Nothing)


-- | Perform a synchronization of the cache with permanent storage once executed the STM transaction
-- when 'syncWrite' policy is `Synchronous`
atomicallySync :: STM a -> IO a
atomicallySync proc=do
   r <- atomically  proc
   sync
   return r

   where
   sync= do
       (savetype,_) <- readIORef tvSyncWrite
       case  savetype of
        Synchronous -> do
            syncCache
        _ -> return ()


-- |Saves the unsaved elems of the cache.
-- Cache writes allways save a coherent state.
--  Unlike `syncChace` this call deletes some elems of  the cache when the number of elems > @sizeObjects@.
--  The deletion depends on the check criteria, expressed by the first parameter.
--  'defaultCheck' is the one implemented to be passed by default. Look at it to understand the clearing criteria.
clearSyncCache ::  (Integer -> Integer-> Integer-> Bool)-> Int -> IO ()
clearSyncCache check sizeObjects= criticalSection saving $ do
      (cache,lastSync) <- readIORef refcache
      t <- timeInteger
      elems <- H.toList cache
      (tosave, elems, size) <- atomically $ extract elems lastSync
      save tosave
      when (size > sizeObjects) $  forkIO (filtercache t cache lastSync elems) >> performGC
      writeIORef refcache (cache, t)


  where

        -- delete elems from the cache according with the checking criteria
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
                              unsafeIOToSTM . H.insert cache key $ CacheElem Nothing w
                              writeTVar tv NotRead
            		      else return ()
    		_    ->  return()



-- | This is a default cache clearance check. It forces to drop from the cache all the
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
     pre    -- !> (concatMap (\(Filtered x) -> keyResource x)tosave)
     mapM (\(Filtered x) -> writeResource x) tosave
     post


data Filtered= forall a.(IResource a)=> Filtered a


extract elems lastSave= filter1 [] [] (0:: Int)  elems
 where
  filter1 sav val n []= return (sav, val, n)
  filter1 sav val n ((_, ch@(CacheElem mybe w)):rest)= do
      mr <- unsafeIOToSTM $ deRefWeak w
      case mr of
        Nothing -> unsafeIOToSTM (finalize w) >> filter1 sav val n rest
        Just (DBRef key  tvr)  ->
         let  tofilter = case mybe of
                    Just _ -> ch:val
                    Nothing -> val
         in do
          r <- readTVar tvr
          case r of
            Exist (Elem r _ modTime) ->
        	  if (modTime >= lastSave)
        	    then filter1 (Filtered r:sav) tofilter (n+1) rest
        	    else filter1 sav tofilter (n+1) rest -- !> ("rejected->" ++ keyResource r)

            _ -> filter1 sav tofilter (n+1) rest


-- | Assures that the IO computation finalizes no matter if the STM transaction
-- is aborted or retried. The IO computation run in a different thread.
-- The STM transaction wait until the completion of the IO procedure (or retry as usual).
--
-- It can be retried if the embedding STM computation is retried
-- so the IO computation must be idempotent.
-- Exceptions are bubbled up to the STM transaction
safeIOToSTM :: IO a -> STM a
safeIOToSTM req= unsafeIOToSTM  $ do
  tv   <- newEmptyMVar
  forkIO $ (req  >>= putMVar  tv . Right)
          `Control.Exception.catch`
          (\(e :: SomeException) -> putMVar tv $ Left e )
  r <- takeMVar tv
  case r of
   Right x -> return x
   Left e -> throw e




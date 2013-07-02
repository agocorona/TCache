-----------------------------------------------------------------------------
--
-- Module      :  Memoization
-- Copyright   :  Alberto GOmez Corona
-- License     :  BSD3
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :  Experimental
-- Portability :  Non portable (uses stablenames)
--
-- |
--
-----------------------------------------------------------------------------
{-# LANGUAGE  DeriveDataTypeable
            , ExistentialQuantification
            , FlexibleInstances
            , TypeSynonymInstances  #-}
module Data.TCache.Memoization (writeCached,cachedByKey,cachedByKeySTM,flushCached,cachedp,addrStr,Executable(..))

where
import Data.Typeable
import Data.TCache
import Data.TCache.Defs(Indexable(..))
import System.Mem.StableName
import System.IO.Unsafe
import System.Time
import Data.Maybe(fromJust)
import Control.Monad.Trans
import Control.Monad.Identity
import Data.RefSerialize(addrHash,newContext)
--import Debug.Trace
--(!>)= flip trace

data Cached a b= forall m.Executable m => Cached a (a -> m b) b Integer deriving Typeable

context= unsafePerformIO newContext

-- | given a string, return a key that can be used in Indexable instances
--  Of non persistent objects, such are cached objects (it changes fron execution to execution)
-- . It uses `addrHash`
addrStr x= "addr" ++ show hash
 where
 hash = case unsafePerformIO $ addrHash context x of
   Right x -> x
   Left x  -> x

-- | to execute a monad for the purpose of memoizing its result
class Executable m where
  execute:: m a -> a

instance Executable IO where
  execute m = unsafePerformIO $ f1 m ""
   where
   f1 m x= m

instance Executable Identity where
  execute (Identity x)= x

instance MonadIO Identity where
  liftIO f=  Identity $!  unsafePerformIO $! f


cachedKeyPrefix = "cached"

instance  (Indexable a) => IResource (Cached a  b) where
  keyResource ch@(Cached a  _ _ _)= cachedKeyPrefix ++ key a   -- ++ unsafePerformIO (addrStr f )

  writeResource _= return ()
  delResource _= return ()
  readResourceByKey k= return Nothing -- error $ "access By key is undefined for cached objects.key= " ++ k


  readResource (Cached a f _ _)=do
   TOD tnow _ <- getClockTime
   let b = execute $ f a
   return . Just $ Cached a f b tnow  -- !> "readRe"

--cache time f a=  do
--   TOD tnow _ <- getClockTime
--   let b = execute $ f a
--   withResources [] . const $ [Cached a f b tnow]     -- !> "writeRe"]
--
--cacheKey key time f= cache time (const  f) key

-- | memoize the result of a computation for a certain time. This is useful for  caching  costly data
-- such  web pages composed on the fly.
--
-- time == 0 means infinite

--getCachedRef :: (Indexable a,Typeable a, Typeable b) => a -> DBRef (Cached a b)
--getCachedRef x = getDBRef $ keyResource (Cached x (u u u) where u= undefined

writeCached
  :: (Typeable b, Typeable a, Indexable a, Executable m) =>
     a -> (a -> m b) -> b -> Integer -> STM ()
writeCached  a b c d=
    withSTMResources [] . const $ resources{toAdd= [Cached a b c d] }


cached ::  (Indexable a,Typeable a,  Typeable b, Executable m,MonadIO m) => Int -> (a -> m b) -> a  -> m b
cached time  f a= liftIO . atomically $ cachedSTM time f a

cachedSTM time f a= do
   let prot= Cached a f undefined undefined
   let ref= getDBRef $ keyResource prot
   cho@(Cached _ _ b t) <- readDBRef ref `onNothing` fillIt ref prot
   case time of
     0 -> return b
     _ -> do
           TOD tnow _ <- unsafeIOToSTM $ getClockTime
           if tnow - t >= fromIntegral time
                      then do
                            Cached _ _ b _ <- fillIt ref prot
                            return b
                      else  return b
   where
   -- has been invalidated by flushCached
   fillIt ref proto= do
     let r = unsafePerformIO $return . fromJust =<< readResource proto   -- !> "fillIt"
     writeDBRef ref r
     return r

-- | Memoize the result of a computation for a certain time. A string 'key' is used to index the result
--
-- The Int parameter is the timeout, in second after the last evaluation, after which the cached value will be discarded and the expression will be evaluated again if demanded
-- . Time == 0 means no timeout
cachedByKey :: (Typeable a, Executable m,MonadIO m) => String -> Int ->  m a -> m a
cachedByKey key time  f = cached  time (\_ -> f) key

cachedByKeySTM :: (Typeable a, Executable m) => String -> Int ->  m a -> STM a
cachedByKeySTM key time  f = cachedSTM  time (\_ -> f) key

-- Flush the cached object indexed by the key
flushCached :: String -> IO ()
flushCached k= atomically $ invalidateKey $ cachedKeyPrefix ++ k           -- !> "flushCached"

-- | a pure version of cached
cachedp :: (Indexable a,Typeable a,Typeable b) => (a ->b) -> a -> b
cachedp f k = execute $ cached  0 (\x -> Identity $ f x) k

--testmemo= do
--   let f x = "hi"++x  !> "exec1"
--   let f1 x= "h0"++x  !> "exec2"
--   let beacon=1
--   let beacon2=2
--   print $ cachedp f (addrStr "sfs")
--   print $ cachedp f (addrStr "sds")
--   print $ cachedp f1 (addrStr "ssdfddd")
--   print $ cachedp f1 (addrStr "sss")



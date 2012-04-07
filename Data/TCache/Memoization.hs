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
{-# LANGUAGE  DeriveDataTypeable  #-}
module Data.TCache.Memoization (cached, tcached)

where
import Data.Typeable
import Data.TCache
import System.Mem.StableName
import System.IO.Unsafe
import System.Time
import Data.Maybe(fromJust)

data Cached a b= Cached a (a -> IO b) b Integer deriving Typeable

class Key a where
  key :: a -> String

instance  (Key a, Typeable a) => IResource (Cached a  b) where
  keyResource ch@(Cached a  f _ _)= key a ++ varName f   --`debug` ("k="++ show k)
    where
    {-# NOINLINE varName #-}
    varName x= show . unsafePerformIO $ (makeStableName $! x) >>= return . hashStableName


  writeResource _= return ()
  delResource _= return ()
  readResourceByKey= error "access By Key is undefined for chached objects"

  readResource (Cached a f _ _)=do
   TOD tnow _ <- getClockTime
   f a >>= \b -> return . Just $ Cached a f b tnow

-- | memoize the result of a computation with a single parameter in the Cache
-- The memoization takes benefit of the cache handling
cached :: (Key a, Typeable a, Typeable b) =>  (a-> IO b) -> a -> IO b
cached f a= do
   Cached _ _ b _  <- getResource ( (Cached a f undefined undefined )) >>= return . fromJust
   return b


-- | memoize the result of a computation for a certain time. This is useful for  caching  heavy pieces of data
-- such are images or web pages composed on the fly.

tcached ::  (Key a, Typeable a, Typeable b) => Int -> (a -> IO b) -> a  -> IO b
tcached time  f a=  do
   cho@(Cached _ _ b t)  <- getResource ( (Cached a f undefined undefined )) >>= return . fromJust
   TOD tnow _ <- getClockTime
   if tnow - t > fromIntegral time then deleteResource cho >> tcached time f a
                      else return b






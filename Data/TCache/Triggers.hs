

{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable, BangPatterns #-}
module Data.TCache.Triggers(DBRef(..),Elem(..),Status(..),addTrigger,applyTriggers) where
import Data.TCache.IResource
import Data.TCache.Defs
import Data.Typeable
import Data.IORef
import System.IO.Unsafe
import Unsafe.Coerce
import GHC.Conc (STM, unsafeIOToSTM)
import Data.Maybe(maybeToList,catMaybes)
import Data.List(nubBy)
import Control.Concurrent.STM

import Debug.Trace
import Data.Maybe(fromJust)

newtype  TriggerType a= TriggerType (DBRef a -> Maybe a -> STM()) deriving Typeable

data CMTrigger= forall a.(IResource a, Typeable a) => CMTrigger  !((DBRef a) -> Maybe a -> STM())



cmtriggers :: IORef [(TypeRep ,[CMTrigger])]
cmtriggers= unsafePerformIO $ newIORef []



{- | Add an user defined trigger to the list of triggers
Trriggers are called just before an object of the given type is created, modified or deleted.
The DBRef to the object and the new value is passed to the trigger.
The called trigger function has two parameters: the DBRef being accesed
(which still contains the old value), and the new value.
If the DBRef is being deleted, the second parameter is 'Nothing'.
if the DBRef contains Nothing, then the object is being created
-}
addTrigger :: (IResource a, Typeable a) => ((DBRef a) -> Maybe a -> STM()) -> IO()
addTrigger   t= do
   map <-  readIORef cmtriggers
   writeIORef cmtriggers $
      let ts = mbToList $ lookup atype map
          in  nubByType $ (atype ,CMTrigger t : ts) : map
  where
  nubByType= nubBy (\(t,_)(t',_) -> t==t')
  (_,(atype:_))= splitTyConApp  . typeOf $ TriggerType t



mbToList mxs= case mxs of Nothing -> []; Just xs -> xs

-- | internally called when a DBRef is modified/deleted/created
applyTriggers:: (IResource a, Typeable a) => [DBRef a] -> [Maybe a] -> STM()
applyTriggers  [] _ = return()
applyTriggers  dbrfs mas = do
   map <- unsafeIOToSTM $ readIORef cmtriggers
   let ts = mbToList $ lookup   (typeOf $ fromJust (head mas)) map
   mapM_ f  ts

   where
   f t= mapM2_ (f1 t)  dbrfs  mas

   f1 ::(IResource a, Typeable a) =>  CMTrigger -> DBRef a -> Maybe a ->  STM()
   f1 (CMTrigger t) dbref ma =    (unsafeCoerce t)  dbref ma



mapM2_ _ [] _= return()
mapM2_ f (x:xs) (y:ys)=  f x y >> mapM2_ f xs ys



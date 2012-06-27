{-# OPTIONS  -XDeriveDataTypeable
             -XTypeSynonymInstances
             -XMultiParamTypeClasses
             -XExistentialQuantification
             -XOverloadedStrings
             -XFlexibleInstances
             -XUndecidableInstances
             -XFunctionalDependencies


             -IControl/Workflow

           #-}

{- |
This module implements a persistent, transactional collection with Queue interface as well as
 indexed access by key

use this version if you store in the queue different versions of largue structures, for
example, documents and define a "Data.RefSerialize" instance. If not, use "Data.Persistent.Queue.Binary" Instead.

Here @QueueConstraints  w r a@ means  @Data.RefSerlialize.Serialize a@

-}

module Data.Persistent.Collection (
RefQueue(..), getQRef,
pop,popSTM,pick, flush, flushSTM,
pickAll, pickAllSTM, push,pushSTM,
pickElem, pickElemSTM,  readAll, readAllSTM,
deleteElem, deleteElemSTM,
unreadSTM,isEmpty,isEmptySTM
) where
import Data.Typeable
import Control.Concurrent.STM(STM,atomically, retry)
import Control.Monad
import Data.TCache.DefaultPersistence

import Data.TCache
import System.IO.Unsafe
import Data.RefSerialize
import Data.ByteString.Lazy.Char8
import Data.RefSerialize

import Debug.Trace

a !> b= trace b a




instance Indexable (Queue a) where
   key (Queue k  _ _)= queuePrefix ++ k




data Queue a= Queue {name :: String, imp :: [a], out ::  [a]}  deriving (Typeable)



instance Serialize a => Serialize (Queue a) where
  showp (Queue n i o)= showp n >> showp i >> showp o
  readp = return Queue `ap` readp `ap` readp `ap` readp
--    do
--       n <-   readp
--       i <-   readp
--       o <-   readp
--       return $ Queue n i o




queuePrefix= "Queue#"
lenQPrefix= Prelude.length queuePrefix



instance   Serialize a => Serializable (Queue a ) where
  serialize = runW . showp
  deserialize = runR  readp

-- | a queue reference
type RefQueue a= DBRef (Queue a)

unreadSTM :: (Typeable a, Serialize a) => RefQueue a -> a -> STM ()
unreadSTM queue x= do
    r <- readQRef queue
    writeDBRef queue $ doit r
    where
    doit (Queue  n  imp out) =   Queue n  imp ( x : out)


-- | check if the queue is empty
isEmpty ::  (Typeable a, Serialize a) => RefQueue a -> IO Bool
isEmpty = atomically . isEmptySTM

isEmptySTM :: (Typeable a, Serialize a) => RefQueue a -> STM Bool
isEmptySTM queue= do
   r <- readDBRef queue
   return $ case r of
        Nothing  ->  True
        Just (Queue _ [] []) -> True
        _    ->  False



-- | get the reference to new or existing queue trough its name
getQRef ::  (Typeable a, Serialize a)  => String -> RefQueue a
getQRef n = getDBRef . key $ Queue n undefined undefined


-- | empty the queue (factually, it is deleted)
flush ::   (Typeable a, Serialize a)  => RefQueue a -> IO ()
flush = atomically . flushSTM

-- | version in the STM monad
flushSTM ::  (Typeable a, Serialize a)  => RefQueue a -> STM ()
flushSTM tv= delDBRef tv

-- | read  the first element in the queue and delete it (pop)
pop
      ::  (Typeable a, Serialize a)  => RefQueue a       -- ^ Queue name
      -> IO a              -- ^ the returned elems
pop tv = atomically $ popSTM tv


readQRef :: (Typeable a, Serialize a)  => RefQueue a -> STM(Queue a)
readQRef tv= do
    mdx <- readDBRef tv
    case mdx of
     Nothing -> do
            let q= Queue ( Prelude.drop lenQPrefix $ keyObjDBRef tv) [] []
            writeDBRef tv q
            return q
     Just dx ->
            return dx

-- | version in the STM monad
popSTM :: (Typeable a, Serialize a) =>  RefQueue a
              -> STM  a
popSTM tv=do
    dx <- readQRef tv
    doit  dx

    where

    doit (Queue n [x] [])= do
                 writeDBRef tv $  (Queue n  [] [])
                 return   x
    doit (Queue _ [] []) =  retry
    doit (Queue  n imp [])  =  doit  (Queue  n [] $ Prelude.reverse imp)
    doit (Queue n imp  list ) = do
                 writeDBRef tv  (Queue  n imp (Prelude.tail list ))
                 return  $ Prelude.head list

--  | read the first element in the queue but it does not delete it
pick
      ::  (Typeable a, Serialize a)  => RefQueue a       -- ^ Queue name
      -> IO a              -- ^ the returned elems
pick tv = atomically $ do
    dx <- readQRef tv
    doit dx
    where
    doit (Queue _ [x] [])= return   x
    doit (Queue _ [] []) =  retry
    doit (Queue  n imp [])  =  doit  (Queue  n [] $ Prelude.reverse imp)
    doit (Queue n imp  list ) = return  $ Prelude.head list

-- | push an element in the queue
push  ::   (Typeable a, Serialize a)  => RefQueue a -> a -> IO ()
push tv v = atomically $ pushSTM tv v

-- | version in the STM monad
pushSTM ::  (Typeable a, Serialize a)  => RefQueue a -> a -> STM ()
pushSTM  tv   v=
      readQRef tv  >>= \ ((Queue n  imp out))  -> writeDBRef tv  $ Queue n  (v : imp) out

-- | return the list of all elements in the queue. The queue remains unchanged
pickAll ::  (Typeable a, Serialize a)  => RefQueue a -> IO [a]
pickAll= atomically  . pickAllSTM

-- | version in the STM monad
pickAllSTM :: (Typeable a, Serialize a)  => RefQueue a -> STM [a]
pickAllSTM tv= do
     (Queue name imp out) <- readQRef tv
     return $ out ++ Prelude.reverse imp

-- | return the first element in the queue that has the given key
pickElem ::(Indexable a,Typeable a, Serialize a) => RefQueue a -> String -> IO(Maybe a)
pickElem tv key= atomically $ pickElemSTM tv key

-- | version in the STM monad
pickElemSTM :: (Indexable a,Typeable a, Serialize a)
                     => RefQueue a -> String -> STM(Maybe a)
pickElemSTM tv key1=  do
     Queue name imp out <- readQRef tv
     let xs= out ++ Prelude.reverse imp
     when (not $ Prelude.null imp) $ writeDBRef tv $ Queue name [] xs
     case  Prelude.filter (\x-> key x == key1) xs of
          []    -> return $ Nothing
          (x:_) -> return $ Just  x

-- | update the first element of the queue with a new element with the same key
updateElem :: (Indexable a,Typeable a, Serialize a)
                    => RefQueue a  -> a -> IO()
updateElem tv x = atomically $ updateElemSTM tv  x

-- | version in the STM monad
updateElemSTM :: (Indexable a,Typeable a, Serialize a)
                       => RefQueue a  -> a -> STM()
updateElemSTM tv v= do
     Queue name imp out <- readQRef tv
     let xs= out ++ Prelude.reverse imp
     let xs'= Prelude.map (\x -> if key x == n then v else x) xs
     writeDBRef tv  $ Queue name [] xs'
     where
     n= key v

-- | return the list of all elements in the queue and empty it
readAll ::  (Typeable a, Serialize a) => RefQueue a -> IO [a]
readAll= atomically  . readAllSTM

-- | a version in the STM monad
readAllSTM ::  (Typeable a, Serialize a)  => RefQueue a -> STM [a]
readAllSTM tv= do
     Queue name imp out <- readQRef tv
     writeDBRef tv  $ Queue name [] []
     return $ out ++ Prelude.reverse imp

-- | delete all the elements of the queue that has the key of the parameter passed
deleteElem :: (Indexable a,Typeable a, Serialize a) => RefQueue a-> a -> IO ()
deleteElem tv x= atomically $ deleteElemSTM tv x

-- | verison in the STM monad
deleteElemSTM :: (Typeable a, Serialize a,Indexable a) => RefQueue a-> a -> STM ()
deleteElemSTM tv x= do
     Queue name imp out <- readQRef tv
     let xs= out ++ Prelude.reverse imp
     writeDBRef tv $ Queue name [] $ Prelude.filter (\x-> key x /= k) xs
     where
     k=key x


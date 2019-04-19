{-# OPTIONS  -XRecordWildCards
             -XTypeSynonymInstances
             -XFlexibleInstances
             -XUndecidableInstances
             -XDeriveDataTypeable  #-}
module Data.Persistent.CollectionStream where
import Data.Typeable
import Control.Concurrent.STM(STM,atomically, retry)
import Control.Monad(when)
import Data.TCache.DefaultPersistence

import Data.TCache
import System.IO.Unsafe
import Data.RefSerialize
import Data.RefSerialize
import Data.ByteString.Lazy.Char8 as B
import qualified Data.Vector as V
import Data.HashTable as HT
import Data.ByteString.Lazy.Char8 as B
import Data.List
import Data.Ord

--import Debug.Trace
--a !> b= trace b a

--assocs = sortBy (comparing fst) . unsafePerformIO . HT.toList
--
--instance Serialize NContext where
--  showp(NContext n (c,s))=
--
--      insertString n
--      insertString $ showContext c
--  readp= do
--    n <- readp
--    s <- readContent
--    let c = unsafePerformIO newContext
--    return $ NContext n (c,s)
--
--
--
--data NContext= NContext String (Context,ByteString) deriving  Typeable
--
--instance Indexable NContext where
--  key (NContext n _)= n

--getExternalContext n= unsafePerformIO . atomically . newDBRef $ NContext n undefined

instance Serialize a => Serializable a where
  serialize= runW . showp
  deserialize= runR  readp

instance Serialize a => Serialize (V.Vector a) where
 showp= showp . V.toList
 readp= readp >>= return . V.fromList




data Elem a=
     Elem{ ename :: String
         , eindex :: Int
         , echilds :: Int
         , emaxlength :: Int
         , enelems :: Int
         , econtent :: V.Vector (Either a, DBRef(Elem a)}

get e = justify (readDBRef rcoll) $ "Not found: " ++ key e

insertByKey re x= do
    e@Elem{..} <- get re
    let Just i' = findIndex (\y -> key y> key x ) econtent
    i= i' -1
    case econtent ! (i-1) of
     Left re' -> insertByKey re' x
     Right t ->
       if (key t== key x)
        then  writeDBRef re e{econtent= econtent //[(i,x)] }
        else do
          toins <-  case enelems < emaxlength
               True -> return $ Right x
               False ->do
                  let childs echilds+1
                  r <- newwDBRef e{eindex= childs+1, childs=0,enelems=1,econtent= singleton x}
                  writeDBRef re e{echilds=childs+1}
                  return $ Left r
          let ncontent= update_
                        econtent
                        (fromList [i..enelems])
                        (cons toins (drop i' econtent))

          writeDBRef re e{econtent=  ncontent}



lookup re k= do
     e@Elem{..} <- get re
     let Just z = findIndex (\y -> key y> k ) econtent
     let i= i' -1
     case econtent ! i of
       Right t -> assert (key t== k) $ return t
       Left re' -> lookup re' x


push re x= do
       e@Elem{..} <- get re
       if emaxlength == enelems


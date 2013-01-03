    {-# OPTIONS -XExistentialQuantification
            -XOverlappingInstances
            -XUndecidableInstances
            -XScopedTypeVariables
            -XDeriveDataTypeable
            -XTypeSynonymInstances
            -XIncoherentInstances
            -XOverloadedStrings
            -XMultiParamTypeClasses
            -XFunctionalDependencies
            -XFlexibleInstances #-}
{- |
IDynamic is a indexable and serializable version of Dynamic. (See @Data.Dynamic@). It is used as containers of objects
in the cache so any new datatype can be incrementally stored without recompilation.
IDimamic provices methods for safe casting,  besides serializaton, deserialirezation and retrieval by key.
-}
module Data.Persistent.IDynamic where
import Data.Typeable
import Unsafe.Coerce
import System.IO.Unsafe
import Data.TCache
import Data.TCache.Defs
import Data.RefSerialize
import Data.Char (showLitChar)

import Data.ByteString.Lazy.Char8 as B

import Data.Word
import Numeric (showHex, readHex)
import Control.Exception(handle, SomeException, ErrorCall)
import Control.Monad(replicateM)
import Data.Word
import Control.Concurrent.MVar
import Data.IORef
import Data.Map as M(empty)
import Data.RefSerialize
import Data.HashTable as HT

--import Debug.Trace
--(!>)= flip trace


data IDynamic  =  IDyn  (IORef IDynType) deriving Typeable

data IDynType= forall a w r.(Typeable a, Serialize a)
               => DRight  !a
               |  DLeft  !(ByteString ,(Context, ByteString))


               deriving Typeable

newtype Save= Save ByteString deriving Typeable

tosave d@(IDyn r)= unsafePerformIO $ do
   mr<- readIORef r
   case mr of
     DRight _ ->  return d
     DLeft (s,_) -> writeIORef r (DRight $ Save s) >> return d


instance Serialize Save  where
  showp (Save s)= insertString s
  readp = error "readp not impremented for Save"


errorfied str str2= error $ str ++ ": IDynamic object not reified: "++ str2



dynPrefix= "Dyn"
dynPrefixSp= append  (pack dynPrefix) " "
notreified = pack $ dynPrefix ++" 0"



instance Serialize IDynamic where

   showp (IDyn t)=
    case unsafePerformIO $ readIORef t of
     DRight x -> do
--          insertString $ pack dynPrefix
          c <- getWContext
          showpx  <-  rshowps x
--          showpText . fromIntegral $ B.length showpx
          showp $ unpack showpx

     DLeft (showpx,_) ->   --  error $ "IDynamic not reified :: "++  unpack showpx
--        insertString   notreified
          insertString  $ encode showpx
            where
            encode =   pack . show . unpack

   readp = lexeme (do
--      symbol dynPrefix
--      n <- readpText
--      s <- takep n

      s <- rreadp :: STR  String

      c <- getRContext
      return . IDyn . unsafePerformIO . newIORef $ DLeft ( pack s, c))
      <?> "IDynamic"



instance Show  IDynamic where
 show (IDyn r) =
    let t= unsafePerformIO $ readIORef r
    in case t of
      DRight x -> "IDyn " ++  ( unpack . runW $ showp  x)
      DLeft (s, _) ->  "IDyns \"" ++ unpack s ++ "\""





toIDyn x= IDyn . unsafePerformIO . newIORef $ DRight x

 
fromIDyn :: (Typeable a , Serialize a)=> IDynamic -> a
fromIDyn x=r where
  r = case safeFromIDyn x of
          Nothing -> error $ "fromIDyn: casting failure for data "
                     ++ show x ++ " to type: "
                     ++ (show $ typeOf r)
          Just v -> v


safeFromIDyn :: (Typeable a, Serialize a) => IDynamic -> Maybe a       
safeFromIDyn (IDyn r)=unsafePerformIO $ do
  t<-  readIORef r
  case t of
   DRight x ->  return $ cast x

   DLeft (str, c) ->
    handle (\(e :: SomeException) ->  return Nothing) $  -- !> ("safeFromIDyn : "++ show e)) $
        do
          let v= runRC  c rreadp str -- !> unpack str
          writeIORef r $! DRight v -- !> ("***reified "++ unpack str)
          return $! Just v -- !>  ("*** end reified " ++ unpack str)


--main= print (safeFromIDyn $ IDyn $ unsafePerformIO $ newIORef $ DLeft $ (pack "1", (unsafePerformIO $ HT.new (==) HT.hashInt, pack "")) :: Maybe Int)

reifyM :: (Typeable a,Serialize a) => IDynamic -> a -> IO a
reifyM dyn v = do
   let v'= fromIDyn dyn
   return $ v' `seq` v'

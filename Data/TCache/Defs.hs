{-# LANGUAGE   DeriveDataTypeable #-}

module Data.TCache.Defs where
import Data.Typeable
import Control.Concurrent.STM(TVar)
	

	
type AccessTime = Integer
type ModifTime  = Integer


data Status a= NotRead | DoNotExist | Exist a deriving Typeable

data Elem a= Elem !a !AccessTime !ModifTime   deriving Typeable

type TPVar a=   TVar (Status(Elem a))

data DBRef a= DBRef !String  !(TPVar a)  deriving Typeable



castErr a= r where
  r= case cast a of
      Nothing -> error $ "Type error: " ++ (show $ typeOf a) ++ " does not match "++ (show $ typeOf r)
                          ++ "\nThis means that objects of these two types have the same key \nor the retrieved object type is not the stored one for the same key\n"
      Just x  -> x


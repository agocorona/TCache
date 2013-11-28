{-# LANGUAGE   FlexibleInstances, UndecidableInstances
               , MultiParamTypeClasses, FunctionalDependencies

               , ExistentialQuantification
               , ScopedTypeVariables
                #-}

{- | This module decouples the 'IResource" class in two classes
 one for key extraction 'Indexable' and other ('Serializable" for serlalization and persistence
 .The last one defines persistence in files as default, but it can be changed
 to persistence in databases, for example.

 The definitions of these classes are  in Defs.hs
-}
module Data.TCache.DefaultPersistence(
Indexable(..)
,Serializable(..)
,setDefaultPersist
,getDefaultPersist
,filePersist
,Persist(..)) where

import System.IO.Unsafe
import Data.Typeable
import Data.Maybe(fromJust)
import Data.TCache.Defs
import Data.TCache




instance  (Typeable a,  Indexable a, Serializable a) => IResource a where
  keyResource = key
  writeResource =defWriteResource
  readResourceByKey = defReadResourceByKey
  delResource = defDelResource

-- | By default the  index of a `Serializable` data persist with the data.
instance Serializable a => PersistIndex a where
   persistIndex= setPersist

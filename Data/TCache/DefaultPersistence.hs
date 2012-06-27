{- |

This module decouples the interface of 'IResource" class in two classes
one for key extraction 'Indexable' and other ('Serializable" for serlalization and persistence
.This last one defines persistence in files as default, but it can be changed
to persistence in databases, for examople.

-}
{-# LANGUAGE   FlexibleInstances, UndecidableInstances
               , MultiParamTypeClasses, FunctionalDependencies

               , ExistentialQuantification
               , ScopedTypeVariables

                #-}
module Data.TCache.DefaultPersistence(Indexable(..),Serializable(..),defaultPersist,Persist(..)) where

import System.IO.Unsafe
import Data.Typeable
import Data.Maybe(fromJust)
import Data.TCache.Defs
import Data.TCache



{- | Indexable is an utility class used to derive instances of IResource

Example:

@data Person= Person{ pname :: String, cars :: [DBRef Car]} deriving (Show, Read, Typeable)
data Car= Car{owner :: DBRef Person , cname:: String} deriving (Show, Read, Eq, Typeable)
@

Since Person and Car are instances of 'Read' ans 'Show', by defining the 'Indexable' instance
will implicitly define the IResource instance for file persistence:

@
instance Indexable Person where  key Person{pname=n} = \"Person \" ++ n
instance Indexable Car where key Car{cname= n} = \"Car \" ++ n
@
-}


instance  (Typeable a,  Indexable a, Serializable a ) => IResource a where

  keyResource = key
  writeResource s=do
      let Persist _ f _ = setPersist  s
      f (defPath s ++ key s) $ castErr $ serialize s

  readResourceByKey k= iox where
    iox= do
      let Persist f _ _ = setPersist  x
      f  file >>= return . fmap  deserialize . castErr
      where
      file= defPath x ++ k
      x= undefined `asTypeOf` (fromJust $ unsafePerformIO iox)

  delResource s= do
      let Persist _ _ f = setPersist s
      f $ defPath s ++ key s



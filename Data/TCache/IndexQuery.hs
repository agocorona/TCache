{- | This module implements an experimental typed query language for TCache build on pure
haskell. It is minimally intrusive (no special data definitions, no special syntax, no template
haskell). It uses the same register fields from the data definitions. Both for query conditions
 and selections. It is executed in haskell, no external database support is needed.

it includes

 - A method for triggering the 'index'-ation of the record fields that you want to query

 - A typed query language of these record fields, with:

     - Relational operators:  '.==.' '.>.' '.>=.' '.<=.' '.<.' '.&&.' '.||.' to compare fields with
       values (returning lists of DBRefs) or fields between them, returning joins (lists of pairs of
       lists of DBRefs that meet the condition).

     - a 'select' method to extract tuples of field values from the  DBRefs

     - a 'recordsWith' clause to extract entire registers

An example that register the owner and name fields fo the Car register and the
name of the Person register, create the Bruce register, return the Bruce DBRef, create two Car registers with bruce as owner
and query for the registers with bruce as owner and its name alpabeticaly higuer than \"Bat mobile\"

@
import "Data.TCache"
import "Data.TCache.IndexQuery"
import "Data.TCache.DefaultPersistence"
import "Data.Typeable"

data Person= Person {pname :: String} deriving  (Show, Read, Eq, Typeable)
data Car= Car{owner :: DBRef Person , cname:: String} deriving (Show, Read, Eq, Typeable)

instance 'Indexable' Person where key Person{pname= n} = \"Person \" ++ n
instance 'Indexable' Car where key Car{cname= n} = \"Car \" ++ n

main =  do
   'index' owner
   'index' pname
   'index' cname
   bruce <- atomically $ 'newDBRef' $ Person \"bruce\"
   atomically $  mapM_ 'newDBRef' [Car bruce \"Bat Mobile\", Car bruce \"Porsche\"]
   r \<- atomically $ cname '.==.' \"Porsche\"
   print r
   r \<- atomically $ 'select' (cname, owner) $  owner '.==.' bruce '.&&.' cname '.>=.' \"Bat Mobile\"
   print r
@

Will produce:

> [DBRef "Car Porsche"]
> [("Porsche",DBRef "Person bruce")]

NOTES:

* the index is instance of 'Indexable' and 'Serializable'. This can be used to
persist in the user-defined storage using DefaultPersistence

* The Join feature has not been properly tested

* Record fields are recognized by its type, so if we define two record fields
with the same type:

> data Person = Person {name , surname :: String}

then a query for @name '.==.' "Bruce"@  is indistinguishable from @surname '.==.' "Bruce"@

Will return indexOf the registers with surname "Bruce" as well. So if two or more
fields in a registers are to be indexed, they must have different types.

-}

{-# LANGUAGE  DeriveDataTypeable, MultiParamTypeClasses
, FunctionalDependencies, FlexibleInstances, UndecidableInstances
, TypeSynonymInstances, IncoherentInstances #-}
module Data.TCache.IndexQuery(
  index
, (.==.)
, (.<.)
, (.<=.)
, (.>=.)
, (.>.)
, indexOf
, recordsWith
, (.&&.)
, (.||.)
, select
, Queriable)
where

import Data.TCache
import Data.TCache.Defs
import Data.List
import Data.Typeable
import Control.Concurrent.STM
import Data.Maybe (catMaybes)
import qualified Data.Map  as M
import Data.IORef
import qualified  Data.Map as M
import System.IO.Unsafe
import Data.ByteString.Lazy.Char8(pack, unpack)


class (Read a,  Show a
      , IResource reg,Typeable reg
      , Typeable a,Ord a,PersistIndex reg)
      => Queriable reg a

instance (Read a,  Show a
      , IResource reg,Typeable reg
      , Typeable a,Ord a,PersistIndex reg)
      => Queriable reg a

instance  Queriable reg a => IResource (Index reg a) where
  keyResource = key
  writeResource =defWriteResource
  readResourceByKey = defReadResourceByKey
  delResource = defDelResource



data Index reg a= Index (M.Map a [DBRef reg]) deriving ( Show, Typeable)

instance (IResource reg, Typeable reg, Ord a, Read a)
   => Read (Index reg a) where
  readsPrec n ('I':'n':'d':'e':'x':' ':str)
     = map (\(r,s) -> (Index r, s)) rs where rs= readsPrec n str
  readsPrec _ s= error $ "indexQuery: can not read index: \""++s++"\""

instance (Queriable reg a) => Serializable (Index reg a)  where
  serialize= pack . show
  deserialize= read . unpack
  setPersist index= persistIndex $ getType index
    where
    getType :: Index reg a -> reg
    getType= undefined -- type level



keyIndex treg tv= "index-" ++ show treg ++ show tv

instance (Typeable reg, Typeable a) => Indexable (Index reg a) where
   key map= keyIndex typeofreg typeofa
       where
       [typeofreg, typeofa]= typeRepArgs $! typeOf map
--   defPath index= defPath $ ofRegister index
--       where
--       ofRegister :: Index reg a -> reg
--       ofRegister = undefined -- type level
-- instance (Queriable reg a, Typeable reg, Typeable a) => IResource (Index reg a) where
--  keyResource = key
--  writeResource =defWriteResource
--  readResourceByKey = defReadResourceByKey
--  delResource = defDelResource

getIndex :: (Queriable reg a)
   => ( reg -> a) -> a -> STM(DBRef (Index reg a), Index reg a,[DBRef reg])
getIndex selector val= do
   let [one, two]= typeRepArgs $! typeOf selector
   let rindex= getDBRef $! keyIndex one two
   getIndexr rindex val


getIndexr :: (Queriable reg a)
   => DBRef(Index reg a) -> a -> STM(DBRef (Index reg a), Index reg a,[DBRef reg])
getIndexr rindex val= do
   mindex <- readDBRef rindex

   let index = case mindex of Just (Index index) ->  index; _ -> M.empty

   let dbrefs= case M.lookup  val index of
        Just  dbrefs -> dbrefs
        Nothing      -> []

   return (rindex, Index index, dbrefs)

selectorIndex
  :: (Queriable reg a, IResource reg
      ) =>
     (reg -> a) -> DBRef (Index reg a) -> DBRef reg -> Maybe reg -> STM ()

selectorIndex selector rindex pobject mobj = do
   moldobj <- readDBRef pobject
   choice moldobj mobj
   where
   choice moldobj mobj=
    case (moldobj, mobj) of
     (Nothing, Nothing) -> return()
     (Just oldobj, Just obj) ->
       if selector oldobj==selector obj
        then return ()
        else do
          choice moldobj Nothing
          choice Nothing mobj

     (Just oldobj, Nothing) -> do  -- delete the old selector value from the index
          let val= selector oldobj
          (rindex,Index index, dbrefs) <-  getIndexr rindex val
          let dbrefs'=   Data.List.delete pobject  dbrefs
          writeDBRef rindex $ Index (M.insert  val dbrefs' index)

     (Nothing, Just obj) ->  do      -- add the new value to the index
          let val= selector obj
          (rindex,Index index, dbrefs) <-  getIndexr rindex val
          let dbrefs'=   nub $ Data.List.insert pobject  dbrefs
          writeDBRef rindex $ Index (M.insert  val dbrefs' index)

{- | Register a trigger for indexing the values of the field passed as parameter.
 the indexed field can be used to perform relational-like searches
-}

index
  :: (Queriable reg a) =>
     (reg -> a) -> IO ()
index sel= do
   let [one, two]= typeRepArgs $! typeOf sel
       rindex= getDBRef $! keyIndex one two
   addTrigger $ selectorIndex sel rindex
   let proto= Index M.empty  `asTypeOf` indexsel sel
   withResources [proto]  $ init proto
   where
   init proto [Nothing]  =  [proto]
   init _ [Just _] = []
   indexsel :: (reg-> a)  -> Index reg a
   indexsel= undefined
-- | implement the relational-like operators, operating on record fields
class RelationOps field1 field2 res | field1 field2 -> res  where
    (.==.) :: field1 -> field2 -> STM  res
    (.>.) :: field1 -> field2 ->  STM  res
    (.>=.):: field1 -> field2 ->  STM  res
    (.<=.) :: field1 -> field2 -> STM  res
    (.<.) :: field1 -> field2 ->  STM  res

-- Instance of relations betweeen fields and values
-- field .op. value
instance (Queriable reg a) => RelationOps (reg -> a) a  [DBRef reg] where
    (.==.) field value= do
       (_ ,_ ,dbrefs) <- getIndex field value
       return dbrefs

    (.>.)  field value= retrieve field value (>)
    (.<.)  field value= retrieve field value (<)
    (.<=.) field value= retrieve field value (<=)

    (.>=.) field value= retrieve field value (>=)

join:: (Queriable rec v, Queriable rec' v)
       =>(v->v-> Bool) -> (rec -> v) -> (rec' -> v) -> STM[([DBRef rec], [DBRef rec'])]
join op field1 field2 =do
  idxs   <- indexOf field1
  idxs' <- indexOf field2
  return $ mix  idxs  idxs'
  where
  opv (v, _ )(v', _)= v `op` v'
  mix    xs  ys=
      let zlist= [(x,y) |  x <- xs , y <- ys, x `opv` y]
      in map ( \(( _, xs),(_ ,ys)) ->(xs,ys)) zlist

type JoinData reg reg'=[([DBRef reg],[DBRef reg'])]

-- Instance of relations betweeen fields
-- field1 .op. field2
instance (Queriable reg a ,Queriable reg' a ) =>RelationOps (reg -> a) (reg' -> a)  (JoinData reg reg') where

    (.==.)= join (==)
    (.>.) = join (>)
    (.>=.)= join (>=)
    (.<=.)= join (<=)
    (.<.) = join (<)

infixr 5 .==., .>., .>=., .<=., .<.

class SetOperations set set'  setResult | set set' -> setResult where
  (.||.) :: STM set -> STM set' -> STM setResult
  (.&&.) :: STM set -> STM set' -> STM setResult


instance SetOperations  [DBRef a] [DBRef a] [DBRef a] where
    (.&&.) fxs fys= do
     xs <- fxs
     ys <- fys
     return $ intersect xs ys

    (.||.) fxs fys= do
     xs <- fxs
     ys <- fys
     return $ union xs ys

infixr 4 .&&.
infixr 3 .||.

instance SetOperations  (JoinData a a') [DBRef a] (JoinData a a') where
    (.&&.) fxs fys= do
     xss <- fxs
     ys <- fys
     return [(intersect xs ys, zs) | (xs,zs) <- xss]

    (.||.) fxs fys= do
     xss <- fxs
     ys <- fys
     return [(union xs ys, zs) | (xs,zs) <- xss]

instance SetOperations  [DBRef a] (JoinData a a')  (JoinData a a') where
    (.&&.) fxs fys=  fys .&&. fxs
    (.||.) fxs fys=  fys .||. fxs

instance SetOperations  (JoinData a a') [DBRef a'] (JoinData a a') where
    (.&&.) fxs fys= do
     xss <- fxs
     ys <- fys
     return [(zs,intersect xs ys) | (zs,xs) <- xss]

    (.||.) fxs fys= do
     xss <- fxs
     ys <- fys
     return [(zs, union xs ys) | (zs,xs) <- xss]


-- |  return all  the (indexed)  values which this field has and a DBRef pointer to the register
indexOf :: (Queriable reg a) => (reg -> a) -> STM [(a,[DBRef reg])]
indexOf selector= do
   let [one, two]= typeRepArgs $! typeOf selector
   let rindex= getDBRef $! keyIndex one two
   mindex <- readDBRef rindex
   case mindex of
     Just (Index index) -> return $ M.toList index;
     _ -> do
        let fields= show $ typeOf  selector
        error $ "the index for "++ fields ++" do not exist. At main, use \"Data.TCache.IdexQuery.index\" to start indexing this field"

retrieve :: Queriable reg a => (reg -> a) -> a -> (a -> a -> Bool) -> STM[DBRef reg]
retrieve field value op= do
   index <- indexOf field
   let higuer = map (\(v, vals) -> if op v value then  vals else [])  index
   return $ concat higuer

-- from a Query result, return the records, rather than the references
recordsWith
  :: (IResource a, Typeable a) =>
     STM [DBRef a] -> STM [ a]
recordsWith dbrefs= dbrefs >>= mapM readDBRef >>= return . catMaybes



class Select  selector a res | selector a -> res  where
  select :: selector -> a -> res


{-
instance (Select sel1 a res1, Select sel2 b res2 )
          => Select (sel1, sel2) (a , b) (res1, res2)  where
  select (sel1,sel2)  (x, y) = (select sel1 x, select sel2 y)
-}


instance (Typeable reg, IResource reg) =>  Select (reg -> a) (STM [DBRef reg])  (STM [a]) where
  select sel xs= return . map sel  =<< return . catMaybes =<< mapM readDBRef  =<< xs


instance  (Typeable reg, IResource reg,
          Select (reg -> a) (STM [DBRef reg])  (STM [a]),
          Select (reg -> b) (STM [DBRef reg])  (STM [b]) )
          =>  Select ((reg -> a),(reg -> b)) (STM [DBRef reg])  (STM [(a,b)])
          where
    select (sel, sel') xs= mapM (\x -> return (sel x, sel' x)) =<< return . catMaybes =<< mapM readDBRef  =<< xs

instance  (Typeable reg, IResource reg,
          Select (reg -> a) (STM [DBRef reg])  (STM [a]),
          Select (reg -> b) (STM [DBRef reg])  (STM [b]),
          Select (reg -> c) (STM [DBRef reg])  (STM [c]) )
          =>  Select ((reg -> a),(reg -> b),(reg -> c)) (STM [DBRef reg])  (STM [(a,b,c)])
          where
    select (sel, sel',sel'') xs= mapM (\x -> return (sel x, sel' x, sel'' x)) =<< return . catMaybes =<< mapM readDBRef  =<< xs


instance  (Typeable reg, IResource reg,
          Select (reg -> a) (STM [DBRef reg])  (STM [a]),
          Select (reg -> b) (STM [DBRef reg])  (STM [b]),
          Select (reg -> c) (STM [DBRef reg])  (STM [c]),
          Select (reg -> d) (STM [DBRef reg])  (STM [d]) )
          =>  Select ((reg -> a),(reg -> b),(reg -> c),(reg -> d)) (STM [DBRef reg])  (STM [(a,b,c,d)])
          where
    select (sel, sel',sel'',sel''') xs= mapM (\x -> return (sel x, sel' x, sel'' x, sel''' x)) =<< return . catMaybes =<< mapM readDBRef  =<< xs

-- for join's   (field1 op field2)

instance  (Typeable reg, IResource reg,
          Typeable reg', IResource reg',
          Select (reg -> a) (STM [DBRef reg])  (STM [a]),
          Select (reg' -> b) (STM [DBRef reg'])  (STM [b]) )
          =>  Select ((reg -> a),(reg' -> b)) (STM (JoinData reg reg')) (STM [([a],[b])])
          where
    select (sel, sel') xss = xss >>=  mapM select1
        where
        select1 (xs, ys) = do
         rxs <- return . map sel  =<< return . catMaybes  =<< mapM readDBRef  xs
         rys <- return .  map sel'  =<< return . catMaybes  =<< mapM readDBRef  ys
         return (rxs,rys)

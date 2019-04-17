{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, UndecidableInstances #-}
module Main where
import Data.TCache
import Data.TCache.DefaultPersistence
import Data.ByteString.Lazy.Char8(pack,unpack)
import GHC.Conc
import System.IO.Unsafe
import Data.Typeable
import Debug.Trace




newtype Other= Other String deriving (Read, Show)

data  Company = Company {
   cname :: String
   ,personnel :: [DBRef Emp]
   ,other :: Other}
   deriving (Read, Show,Typeable)


data Emp= Emp{ename :: String, salary :: Float} deriving (Read, Show, Typeable)

instance Indexable Company where
  key Company{cname=name}= name

instance (Read a, Show a) => Serializable a where
   serialize= pack . show
   deserialize= read . unpack

instance Indexable Emp where
  key Emp{ename= name}= name


myCompanyName= "mycompany"

{-# NOINLINE myCompanyRef #-}
myCompanyRef= unsafePerformIO . atomically $  do

     refEmp1 <- newDBRef Emp{ename= "Emp1", salary= 34000}
     refEmp2 <- newDBRef Emp{ename= "Emp2", salary= 35000}
     refEmp3 <- newDBRef Emp{ename= "Emp3", salary= 54000}
     refEmp4 <- newDBRef Emp{ename= "Emp4", salary= 64000}

     newDBRef $
           Company
               {cname= myCompanyName
               ,personnel= [refEmp1, refEmp2, refEmp3, refEmp4]
               ,other= Other "blah blah blah"}


-- myCompany= Company myCompanyName [getDBRef "Emp1",getDBRef "Emp2",getDBRef "Emp3"]



increaseSalaries percent= do
  mycompany' <- readDBRef myCompanyRef
  mycompany <- case mycompany' of
    Just x -> pure x
    Nothing -> error "Boom"

  mapM_  (increase percent ) $ personnel mycompany
  where
  increase percent ref= do
    emp' <- readDBRef ref
    emp <- case emp' of
      Just x -> pure x
      Nothing -> error "Boom"

    writeDBRef ref $ emp{salary= salary emp * factor}
    where
    factor= 1+ percent/ 100

printSalaries ref= do
  Just comp <- atomically $ readDBRef ref
  mapM_ printSalary $ personnel comp
  where
  printSalary ref= atomically (readDBRef ref) >>=  print

putMsg msg= putStrLn $ ">>" ++ msg

main= do
  putMsg "DBRefs are cached indexable, serializable, unique-by-key references to objects stored in the cache, mutable under STM transactions"
  putMsg "DBRef's are instances of Show"
  print myCompanyRef


  let myCompanyRef2= read $ show myCompanyRef :: DBRef Company
  putMsg "DBRefs are identified by the key of the referenced object"
  putMsg "DBRef's are alse instances of read"

  print myCompanyRef2
  putMsg "DBReference's with the same key point to the same data object"
  putMsg "DBRefs can be part of serializable mutable structures"
  putMsg "the referenced object are reloaded  transparently on demand in the cache and discarded according with TCache definable policies"
  putMsg "the DBRef load and reload requires a cache lockup, but subsequient accesses does not. so performance is almost like TVars and way better that the *Resource* primitives"
  atomically (readDBRef myCompanyRef) >>= print
  atomically (readDBRef myCompanyRef2) >>= print

  putMsg "Before salary increase, the company personnel is accessed with the second reference"
  printSalaries myCompanyRef2
  putMsg "atomically increase the salaries of all the personel"
  atomically $ increaseSalaries 10
  putMsg "after the increase"
  printSalaries myCompanyRef2

  let emp3ref= getDBRef "Emp3"
  putMsg "tch tch, this bad boy does not deserve his salary"
  Just emp3 <- atomically $ readDBRef emp3ref
  print emp3
  atomically $ writeDBRef emp3ref $ emp3{salary= 10000}

  putMsg "so the complete list of company salaries are..."
  printSalaries myCompanyRef
  syncCache  --  use it if you want to save all the changes. (or, else, clearSyncCache)

  putStrLn "checking race condition on cache cleaning"

  let emp1=  Emp{ename="Emp1", salary= -1}
  let key= keyResource emp1
  let remp1 = getDBRef key
  Just emp1 <- atomically $ readDBRef remp1
  atomically $ flushDBRef  remp1
  let remp1'= getDBRef key
  atomically $ writeDBRef remp1' $ emp1{salary=0}

  putStrLn "must reflect the salary 0 for emp1"
  printSalaries myCompanyRef2





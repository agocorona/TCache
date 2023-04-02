example of trigger and DBRef usage to maintain data relations
This is an example of trigger usage, but is a bad practice
For a better implementation of two way references, with this same example
 see indexQuery.hs that uses Data.TCache.IndexQuery

mimic the example taken from http://docs.yesodweb.com/book/persistent

implements one to many relationships with the use of DBref's and triggers to maintain
the relationships.


> {-# LANGUAGE DeriveDataTypeable, FlexibleInstances, UndecidableInstances#-}
> module Main where
> import Data.TCache
> import Data.TCache.Triggers
> import Data.TCache.DefaultPersistence
> import Data.ByteString.Lazy.Char8(pack,unpack)
> import Control.Concurrent.STM
> import Data.List (delete,nub)
> import Data.Typeable
> import Control.Monad(when)
> import Debug.Trace
> import GHC.Conc


> data Person= Person{ pname :: String, cars :: [DBRef Car]} deriving (Show, Read, Typeable)
> data Car= Car{owner :: DBRef Person , cname:: String} deriving (Show, Read, Eq, Typeable)

> instance Indexable Person where  key Person{pname=n} = "Person " ++ n
> instance Indexable Car where key Car{cname= n} = "Car " ++ n

> instance (Read a, Show a) => Serializable a where
>    serialize= pack . show
>    deserialize= read . unpack

every time a car is added, or deleted the owner's list is updated
this is done by the addCar trigger

> addCar pcar (Just(car@(Car powner _ ))) = addToOwner powner pcar
> addCar pcar Nothing  = readDBRef pcar >>= \(Just car)-> deleteOwner (owner car) pcar

> addToOwner powner pcar=do
>    Just owner <- readDBRef powner
>    writeDBRef powner owner{cars= nub $ pcar : cars owner}

> deleteOwner powner pcar= do
>   Just owner <- readDBRef powner
>   writeDBRef powner owner{cars= Data.List.delete  pcar $ cars owner}



> main= do
>    addTrigger addCar
>    putStrLn "create bruce's register with no cars"
>    bruce <- atomically $ newDBRef $ Person "Bruce" []

>    putStrLn "add two car register with \"bruce\" as owner using the reference to the bruces register"
>    let newcars= [Car bruce "Bat Mobile" , Car bruce "Porsche"]
>    insert newcars


>    Just bruceData <- atomically $ readDBRef bruce
>    putStrLn "the trigger automatically updated the car references of the Bruce register"
>    print . length $ cars bruceData
>    print bruceData
>
> insert= withResources [] . const

gives:

 >main1
 >2
 >Person {pname = "Bruce", cars = [DBRef "Car Porsche",DBRef "Car Bat Mobile"]}
- -


the car can be sold, and the modification of the car register can come from
an  owner change. The deletion of the car from the previous owner list can be
done because the trigger hook is called before the update, so
the DBRef maintain the old value.

So a better version of addCar is:


> betterAddCar pcar car@(Car powner _ ) = do
>    updateNeeded <- processOldOwner powner pcar           -- inserted
>    when updateNeeded $ addToOwner powner pcar
>    where
>    processOldOwner powner pcar= do
>     mc <- readDBRef pcar
>     case mc of
>       Nothing -> return True
>       Just Car{owner= pother} ->
>         if pother== powner
>          then return False   -- is unchanged, not necessary to update
>          else do
>           deleteOwner pother  pcar
>           return True


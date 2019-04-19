 {-# LANGUAGE   ScopedTypeVariables
    , UndecidableInstances, FlexibleInstances #-}
module Data.TCache.IResource where


{- | Must be defined for every object to be cached.
-}
class IResource a where
        {- The `keyResource string must be a unique  since this is used to index it in the hash table.
        when accessing a resource, the user must provide a partial object for wich the key can be obtained.
        for example:

        @data Person= Person{name, surname:: String, account :: Int ....)

        keyResource Person n s ...= n++s@

        the data being accesed must define the fields used by keyResource. For example

         @  readResource Person {name="John", surname= "Adams"}@

        leaving the rest of the fields undefined

        when using default file persistence, the key is used as file name. so it must contain valid filename characters

        -}
        keyResource :: a -> String             -- ^ must be defined

        {- | Implements the database access and marshalling of the object.
        while the database access must be strict, the marshaling must be lazy if, as is often the case,
        some parts of the object are not really accesed.
        If the object contains DBRefs, this avoids unnecesary cache lookups.
        This method is called within 'atomically' blocks.
        Since STM transactions retry, readResourceByKey may be called twice in strange situations. So it must be idempotent, not only in the result but also in the effect in the database
        . However, because it is executed by 'safeIOToSTM' it is guaranteed that the execution is not interrupted.
        -}
        readResourceByKey :: String -> IO(Maybe a)
        readResourceByKey k= head <$> readResourcesByKey [k]
        -- | hopefully optimized read of many objects by key.
        readResourcesByKey :: [String] -> IO [Maybe a]
        readResourcesByKey = mapM readResourceByKey

        -- To allow accesses not by key but by any criteria based on the content of the record fields
        -- included in the -partial- definition of the input record. (it defaults as @readResourceByKey $ keyResource x@)
        readResource :: a -> IO (Maybe a)
        readResource x = readResourceByKey $ keyResource x

        -- | To write into persistent storage. It must be strict.
        -- Since STM transactions may retry, @writeResource@ must be idempotent, not only in the result but also in the effect in the database.
        -- . However, because it is executed by 'safeIOToSTM' it is guaranteed that the execution is not interrupted.
        -- All the new obbects are writeen to the database on synchromization,
        -- so writeResource must not autocommit.
        -- Commit code must be located in the postcondition. (see  `setConditions`)
        -- Since there is no provision for rollback from failure in writing to
        -- persistent storage, 'writeResource' must retry until success.
        writeResource:: a-> IO()
        writeResource r= writeResources [r]

        -- | multiple write (hopefully) in a single request. That is up to you and your backend
        -- . Defined by default as 'mapM_ writeResource'
        writeResources :: [a] -> IO()
        writeResources= mapM_ writeResource

        -- | Delete the resource. It is called syncronously. So it must commit
        delResource:: a-> IO()
        delResource x= delResources [x]

        delResources :: [a] -> IO()
        delResources= mapM_ delResource
-- | Resources data definition used by 'withSTMResources'
data Resources a b
       = Retry             -- ^ forces a retry
       | Resources
          { toAdd :: [a]    -- ^ resources to be inserted back in the cache
          , toDelete :: [a] -- ^ resources to be deleted from the cache and from permanent storage
          , toReturn :: b   -- ^ result to be returned
          }


-- | Empty resources: @resources= Resources  [] [] ()@
resources :: Resources a ()
resources =  Resources  [] [] ()

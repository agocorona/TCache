* 0.13.x : (WIP) Major refaktoring and cleanups may break older code
* 0.12.1.0 : Dropped Data.Hashtable (deprecated). Now it uses the package hashtables
* 0.12.0.0 : space in index data in indexQuery.hs and IndexText.hs triggered errors in the AWS backend.  The space has been changed by \'-\'. So rename the "index *" files in the TCache folder in order to be recognized.  
* 0.11.0.0 : added setIndexParsist to define persistence for indexes by type. started the addition of readDBRefs, readResources and so on for simultaneous read, writes and deletes of objects of the same type.
* 0.10.2.0 : Added setDefaultPersist and modified the signature of setPersist in Data.TCache.DefaultPersistence. Fixed issues with ghc 7.6.3
* 0.10.0.0 : version add memoization and a persistent and transactional collection/queue.
* 0.10.0.8 : subversion add cachedByKeySTM
* 0.10.0.9 : fixed an error in clearSyncCacheProc and SynWrite Asynchronous that checked the cache continuously
* 0.9.0.4 : Solves a bug in the management of weak pointers that evaporated registers from the cache
* 0.9.0.3 : Solves a lost registers bug.
* 0.9.0.1 : Solves a bug when object keys generate invalid filenames, and includes changes in defaultPersistence to further separate serialization from input-output.
* 0.9.0.0 : Adds full-text indexing and search, which is incorporated into the experimental query language. It also changes the default Persistence mechanism. Now `ByteString`s are used for serialization and deserialization. A `Serializable` class and a `Persist` structure decouples serialization from `ByteString` and read/write to files. Both can be redefined separately, so the default persistence could be changed with `setPersist` to write to blobs in a databases, for example. Default persistence now no longer has to be in files.

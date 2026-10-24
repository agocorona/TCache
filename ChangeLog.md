* 1.0.0 : Breaking changes (hence the major version):
  - The name of the files of the indexes of `Data.TCache.IndexQuery` has changed: from `index-<type><field>` to `index/<type>-<field>`.
    The index files created by previous versions are not found by this one.
  - `Show` of `DBRef` writes `DB# "key"` instead of `DBRef "key"`. `Read` accepts both formats, so the data written by previous
    versions can be read, but the data written by this version can not be read by previous ones.
  - The functions of `Data.TCache.IndexQuery` (`index`, `getIndex`, `join`, the relational operators...) and the instance
    `IResource (Index reg a)` now require `Indexable reg`.
  New:
  - `referencedKeys`, a new method of `IResource` (default: no references): the keys of the DBRefs that a record contains.
  - `syncDBRef`: saves a DBRef and, recursively, the DBRefs that it references (cycles are handled).
  - `existDataForKey`: tells if there are data for a key, in the cache or in the permanent storage, without adding it to the cache.
  Internal: `safeIOToSTM` uses `forkFinally`; `takeDBRef` reads the resource by key.
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

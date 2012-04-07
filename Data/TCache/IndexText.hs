{-# LANGUAGE TypeSynonymInstances
             , DeriveDataTypeable
             , FlexibleInstances
             , UndecidableInstances
             , MultiParamTypeClasses #-}


{- | Implements full text indexation (`indexText`) and text search(`contains`), as an addition to
the query language implemented in `Data.TCache.IndexQuery`
it also can index the lists of elements in a field (with `indexList`)
so that it is possible to ask for the registers that contains a given element
in the given field (with `containsElem`)

An example of full text search i before and after an update in the text field


@
data Doc= Doc{title, body :: String} deriving (Read,Show, Typeable)
instance Indexable Doc where
  key Doc{title=t}= t

instance Serializable Doc  where
  serialize= pack . show
  deserialize= read . unpack

main= do
  'indexText'  body T.pack
  let doc= Doc{title=  "title", body=  \"hola que tal estamos\"}
  rdoc <- atomically $ newDBRef doc
  r1 <- atomically $ select title $ body \`'contains'\` \"hola que tal\"
  print r1

  atomically $ 'writeDBRef' rdoc  doc{ body=  "que tal"}
  r <- atomically $ 'select' title $ body  \`'contains'\` \"hola que tal\"
  print r
  if  r1 == [title doc] then print \"OK\" else print \"FAIL\"
  if  r== [] then print \"OK\" else print \"FAIL\"
@
-}

module Data.TCache.IndexText(indexText, indexList,  contains, containsElem) where
import Data.TCache
import Data.TCache.IndexQuery
import Data.TCache.DefaultPersistence
import qualified Data.Text.Lazy as T
import Data.Typeable
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Bits
import System.Mem.StableName
import Data.List((\\))
import GHC.Conc(unsafeIOToSTM)
import Control.Concurrent(forkIO)
import Data.Char
import Control.Concurrent(threadDelay)
import Data.ByteString.Lazy.Char8(pack, unpack)
--import Debug.Trace

data IndexText=  IndexText
        { fieldType :: !String
        , lastDoc :: Int
        , mapDocKeyInt :: M.Map String Int
        , mapIntDocKey :: M.Map Int String
        , mapTextInteger :: M.Map T.Text Integer

         } deriving (Typeable)


instance Show IndexText  where
   show (IndexText t a b c d)= show (t,a,b,c,d)

instance Read IndexText  where
  readsPrec n str= [(IndexText t a b c d, str2)| ((t,a,b,c,d),str2) <- readsPrec n str]

instance Serializable IndexText  where
  serialize= pack . show
  deserialize= read . unpack

instance  Indexable IndexText  where
   key (IndexText v _ _ _ _)=    "IndexText " ++ v


readInitDBRef v x= do
  mv <- readDBRef x
  case mv of
    Nothing -> writeDBRef x v >> return v
    Just v -> return v

add ref t key w = op ref t setBit w key
del ref t key w = op ref t clearBit w key

op refIndex t set w key =  do
 if T.length w <3
  then return ()
  else do
   mindex <- readDBRef refIndex
   case mindex of
       Nothing -> writeDBRef refIndex $ IndexText t 0 (M.singleton key 0) (M.singleton  0 key) (M.singleton w 1)
       Just (IndexText t n mapSI mapIS map) -> do
        let (docLocation,n', mapSI')= case M.lookup key mapSI  of
               Nothing  -> let n'= n+1 in (n', n', M.insert key n' mapSI)  -- new Document
               Just m -> (m,n, mapSI)         -- already indexed document

        case M.lookup w map of
         Nothing ->    --new word
            writeDBRef refIndex $ IndexText t  n mapSI mapIS (M.insert w (set 0 docLocation) map)
         Just integer ->  -- word already indexed
               writeDBRef refIndex $ IndexText t n mapSI mapIS $ M.insert w (set integer docLocation) map

-- | start a trigger to index the contents of a register field
indexText
  :: (IResource a, Typeable a, Typeable b)
     => (a -> b)      -- ^ field to index
     -> (b -> T.Text) -- ^ method to convert the field content to lazy Text (for example `pack` in case of String fields). This permits to index non Textual fields
     -> IO ()
indexText sel convert= addTrigger (indext sel  (words1 . convert)) where

-- | trigger the indexation of list fields with elements convertible to Text
indexList
  :: (IResource a, Typeable a, Typeable b)
     => (a -> b)      -- ^ field to index
     -> (b -> [T.Text]) -- ^ method to convert a field element to Text (for example `pack . show` in case of elemets with Show instances)
     -> IO ()
indexList sel convert= addTrigger (indext sel  convert) where


indext :: (IResource a, Typeable a,Typeable b)
       => (a -> b) -> (b -> [T.Text])  -> DBRef a -> Maybe a -> STM()
indext sel  convert dbref  mreg= f1 --  unsafeIOToSTM $! f
  where
  f=  forkIO (atomically f1) >> return()
  f1=  do
   moldreg <- readDBRef dbref
   case ( moldreg,  mreg) of
      (Nothing, Just reg)    -> mapM_ (add refIndex t (keyResource reg))    .  convert $ sel reg
      (Just oldreg, Nothing) -> mapM_ (del refIndex t (keyResource oldreg)) .  convert $ sel oldreg
      (Just oldreg, Just reg) -> do
        st  <- unsafeIOToSTM $ makeStableName $ sel oldreg -- test if field
        st' <- unsafeIOToSTM $ makeStableName $ sel reg    -- has changed
        if st== st'
          then return ()
          else do
            let key= keyResource reg
            let wrds = convert $ sel oldreg
            let wrds'= convert $ sel reg
            let new=  wrds' \\ wrds
            let old= wrds \\ wrds'
            mapM (del refIndex t key) old
            mapM (add refIndex t key) new
            return()
   where
   [t1,t2]=  typeRepArgs $! typeOf sel
   t=  show t1 ++ show t2
   refIndex= getDBRef . key $ IndexText t u u u u where u= undefined

containsElem :: (IResource a, Typeable a, Typeable b) => (a -> b)  -> String -> STM [DBRef a]
containsElem  sel wstr = do
    let w= T.pack wstr
    let [t1, t2]=  typeRepArgs $! typeOf sel
    let t=  show t1 ++ show t2
    let u= undefined
    mr <- withSTMResources [IndexText t u u u u]
       $ \[r] -> resources{toReturn= r}
    case mr of
      Nothing -> return []
      Just (IndexText t n _ mmapIntString map1) ->
       case M.lookup w map1 of
        Nothing ->  return []
        Just integer ->  do
            let mns=map (\n ->case testBit integer n of True -> Just n; _ -> Nothing)  [0..n]
            let wordsr = catMaybes $ map (\n -> M.lookup n mmapIntString) $ catMaybes mns
            return $ map getDBRef wordsr

words1= T.split (\c -> isSeparator c || c=='\n' || isPunctuation c )

-- | return the DBRefs whose fields include all the words of length three or more in the requested text contents
contains
  :: (IResource a, Typeable a, Typeable b)
  =>( a -> b)      -- ^ field to search in
   -> String       -- ^ text to search
   -> STM [DBRef a]
contains sel str= case  words str of
     [] -> return []
     [w] -> containsElem sel w
     ws  -> do
        let rs = map (containsElem sel) $ filter (\t -> length t >2) ws
        foldl (.&&.) (head rs)  (tail rs)




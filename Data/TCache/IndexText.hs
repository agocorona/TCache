{-# LANGUAGE DeriveDataTypeable, FlexibleInstances,
  UndecidableInstances, MultiParamTypeClasses #-}


{- | Implements full text indexation (`indexText`) and text search(`contains`), as an addition to
the query language implemented in `Data.TCache.IndexQuery`
it also can index the lists of elements in a field (with `indexList`)
so that it is possible to ask for the registers that contains a given element
in the given field (with `containsElem`)

An example of full text search and element search in a list in combination
using the `.&&.` operator defined in "indexQuery".
before and after the update of the register


@
data Doc= Doc{title :: String , authors :: [String], body :: String} deriving (Read,Show, Typeable)
instance Indexable Doc where
  key Doc{title=t}= t

instance Serializable Doc  where
  serialize= pack . show
  deserialize= read . unpack

main= do
  'indexText'  body T.pack
  'indexList' authors  (map T.pack)

  let doc= Doc{title=  \"title\", authors=[\"john\",\"Lewis\"], body=  \"Hi, how are you\"}
  rdoc <- atomically $ newDBRef doc

  r0 <- atomically $ `select` title $ authors \``containsElem`\` \"Lewis\"
  print r0

  r1 <- atomically $ `select` title $ body \``contains`\` \"how are you\"
  print r1

  r2 <- atomically $ `select` body $ body \``contains`\` \"how are you\" .&&. authors `containsElem` "john"
  print r2

  atomically $ writeDBRef rdoc  doc{ body=  \"what's up\"}

  r3 <- atomically $ 'select' title $ body  \`'contains'\` \"how are you\"
  print r3

  if  r0== r1 && r1== [title doc] then print \"OK\" else print \"FAIL\"
  if  r3== [] then print \"OK\" else print \"FAIL\"
@



-}

module Data.TCache.IndexText(
  indexText
, indexList
, contains
, containsElem
, allElemsOf) where
import Data.TCache
import Data.TCache.IndexQuery
import Data.TCache.Defs
import qualified Data.Text.Lazy as T
import Data.Typeable
import qualified Data.Map as M
import Data.Maybe
import Data.Bits
import System.Mem.StableName
import Data.List((\\))
import GHC.Conc(unsafeIOToSTM)
import Data.Char
import Data.ByteString.Lazy.Char8(pack, unpack)
import Control.Monad

--import Debug.Trace
--(!>)= flip trace

data IndexText =  IndexText
  !String -- fieldType
  Int -- lastDoc
  (M.Map String Int) -- mapDocKeyInt
  (M.Map Int String) -- mapIntDocKey
  (M.Map T.Text Integer) -- mapTextInteger
  deriving (Typeable)


instance Show IndexText  where
   show (IndexText t a b c d)= show (t,a,b,c,d)

instance Read IndexText  where
  readsPrec n str= [(IndexText t a b c d, str2)| ((t,a,b,c,d),str2) <- readsPrec n str]

instance Serializable IndexText  where
  serialize= pack . show
  deserialize= read . unpack
  setPersist=  const Nothing

instance  Indexable IndexText  where
   key (IndexText v _ _ _ _)=    "indextext-" ++ v

instance IResource IndexText where
  keyResource = key
  writeResource =defWriteResource
  readResourceByKey = defReadResourceByKey
  delResource = defDelResource

{-
readInitDBRef v x= do
  mv <- readDBRef x
  case mv of
    Nothing -> writeDBRef x v >> return v
    Just v -> return v
-}

add :: DBRef IndexText -> String -> String -> [T.Text] -> STM ()
add ref t key1 w = op ref t setBit w key1

del :: DBRef IndexText -> String -> String -> [T.Text] -> STM ()
del ref t key1 w = op ref t clearBit w key1

op :: DBRef IndexText -> String -> (Integer -> Int -> Integer) -> [T.Text] -> String -> STM ()
op refIndex t set ws1 key1 =  do
   mindex <- readDBRef refIndex
   let mindex'= process mindex  ws1
   writeDBRef refIndex $ fromJust mindex'

 where
 process mindex []= mindex
 process mindex (w:ws) =
   case mindex of
       Nothing ->  process (Just $ IndexText t 0 (M.singleton key1 0) (M.singleton  0 key1) (M.singleton w 1)) ws
       Just (IndexText _ n mapSI mapIS map1) -> do
        let (docLocation, n1, mapSI',mapIS')= case M.lookup key1 mapSI  of
               Nothing  -> let n2= n+1 in (n2, n2
                                          , M.insert key1 n2 mapSI
                                          , M.insert n2 key1 mapIS)  -- new Document
               Just m -> (m,n, mapSI,mapIS)         -- already indexed document

        case M.lookup w map1 of
         Nothing ->    --new word
            process (Just $ IndexText t  n1 mapSI' mapIS' (M.insert w (set 0 docLocation) map1)) ws
         Just integer ->  -- word already indexed
            process (Just $ IndexText t n1 mapSI' mapIS' $ M.insert w (set integer docLocation) map1) ws

addProto :: Typeable a => a -> IO ()
addProto sel =  do
  let [t1,t2]=  typeRepArgs $! typeOf sel
  let t =  show t1 ++ show t2
  let proto = IndexText t 0 M.empty M.empty M.empty
  withResources [proto] $ init' proto
  where
   init' proto [Nothing]  = [proto]
   init' _ [Just _] = []
   init' _ [] = error "this will never happen(?)"
   init' _ (Nothing:_:_) = error "this will never happen(?)"
   init' _ (Just _:_:_) = error "this will never happen(?)"

-- | start a trigger to index the contents of a register field
indexText
  :: (IResource a, Typeable a, Typeable b)
     => (a -> b)      -- ^ field to index
     -> (b -> T.Text) -- ^ method to convert the field content to lazy Text (for example `pack` in case of String fields). This permits to index non Textual fields
     -> IO ()
indexText sel convert= do
  addTrigger (indext sel  (words1 . convert))
  addProto sel

-- | trigger the indexation of list fields with elements convertible to Text
indexList
  :: (IResource a, Typeable a, Typeable b)
     => (a -> b)      -- ^ field to index
     -> (b -> [T.Text]) -- ^ method to convert a field element to Text (for example `pack . show` in case of elements with Show instances)
     -> IO ()
indexList sel convert= do
  addTrigger (indext sel  convert)
  addProto sel

indext :: (IResource a, Typeable a,Typeable b)
       => (a -> b) -> (b -> [T.Text])  -> DBRef a -> Maybe a -> STM()
indext sel convert dbref mreg = f1 --  unsafeIOToSTM $! f
  where
    {-f = void $ forkIO (atomically f1)-}
    f1 = do
      moldreg <- readDBRef dbref
      case (moldreg, mreg) of
        (Nothing, Just reg) -> add refIndex t (keyResource reg) . convert $ sel reg
        (Just oldreg, Nothing) -> del refIndex t (keyResource oldreg) . convert $ sel oldreg
        (Just oldreg, Just reg) -> do
          st <- unsafeIOToSTM $ makeStableName $ sel oldreg -- test if field
          st' <- unsafeIOToSTM $ makeStableName $ sel reg -- has changed
          if st == st'
            then return ()
            else do
              let key1 = keyResource reg
              let wrds = convert $ sel oldreg
              let wrds' = convert $ sel reg
              let new = wrds' \\ wrds
              let old = wrds \\ wrds'
              unless (null old) $ del refIndex t key1 old
              unless (null new) $ add refIndex t key1 new
        (Nothing, Nothing) -> error "this will never happen(?)"
      where
        [t1, t2] = typeRepArgs $! typeOf sel
        t = show t1 ++ show t2
        refIndex = getDBRef . key $ IndexText t u u u u
          where
            u = undefined

-- avoid duplicate code
targs :: Typeable a => a -> STM (Maybe IndexText)
targs sel = do
  let [t1, t2]=  typeRepArgs $! typeOf sel
  let t=  show t1 ++ show t2
  let u= undefined
  withSTMResources [IndexText t u u u u]
     $ \[r] -> resources{toReturn= r}

-- | return the DBRefs of the registers whose field (first parameter, usually a container) contains the requested value.
containsElem :: (IResource a, Typeable a, Typeable b) => (a -> b)  -> String -> STM [DBRef a]
containsElem sel wstr = do
  let w = T.pack wstr
  mr <- targs sel
  case mr of
    Nothing -> do
      let fields = show $ typeOf sel
      error $
        "the index for " ++
        fields ++ " do not exist. At main, use \"Data.TCache.IndexQuery.index\" to start indexing this field"
    Just (IndexText _ n _ mmapIntString map1) ->
      case M.lookup w map1 of
        Nothing -> return []
        Just integer -> do
          let mns =
                map
                  (\i ->
                     if testBit integer i
                       then Just i
                       else Nothing)
                  [0 .. n]
          let wordsr = mapMaybe (`M.lookup` mmapIntString) $ catMaybes mns
          return $ map getDBRef wordsr

-- | return all the values of a given field (if it has been indexed with 'index')
allElemsOf :: (IResource a, Typeable a, Typeable b) => (a -> b) -> STM [T.Text]
allElemsOf  sel  = do
    mr <- targs sel
    case mr of
      Nothing -> return []
      Just (IndexText _ _ _ _ map') -> return $ M.keys map'

words1 :: T.Text -> [T.Text]
words1 = filter filterWordt {-( (<) 2 . T.length)-} . T.split (\c -> isSeparator c || c=='\n' || isPunctuation c )

-- | return the DBRefs whose fields include all the words in the requested text contents.Except the
-- words  with less than three characters that are not digits or uppercase, that are filtered out before making the query
contains
  :: (IResource a, Typeable a, Typeable b)
  =>( a -> b)      -- ^ field to search in
   -> String       -- ^ text to search
   -> STM [DBRef a]
contains sel str= case  words str of
     [] -> return []
     [w] -> containsElem sel w
     ws  -> do
        let rs = map (containsElem sel) $ filter filterWord ws
        foldl1 (.&&.) rs

filterWordt :: T.Text -> Bool
filterWordt w = T.length w >2 || any (\c -> isUpper c || isDigit c) (T.unpack w)

filterWord :: Foldable t => t Char -> Bool
filterWord w = length w >2 || any (\c -> isUpper c || isDigit c) w

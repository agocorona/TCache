{-# LANGUAGE  FlexibleInstances
              ,UndecidableInstances
              ,DeriveDataTypeable
               #-}

import Data.TCache
import Data.TCache.DefaultPersistence
import Data.Typeable
import  Data.ByteString.Lazy.Char8 as B

type UserName=  String

data User= User
            { userName :: UserName
            , upassword :: String
            } deriving (Read, Show, Typeable)

userPrefix= "User#"
instance Indexable User where
   key User{userName=   user}= userPrefix++user



userRegister :: String -> String  -> IO(DBRef User)
userRegister user password  = atomically $ newDBRef $ User user password

instance (Show a, Read a)=> Serializable a where
  serialize= pack . show
  deserialize= read . unpack

main :: IO ()
main = do
   userRegister "test" "12345678"
   print "(WIP)"

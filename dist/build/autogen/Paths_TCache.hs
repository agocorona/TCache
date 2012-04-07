module Paths_TCache (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,9,0,4], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "C:\\Users\\agocorona\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\agocorona\\AppData\\Roaming\\cabal\\TCache-0.9.0.4\\ghc-7.0.3"
datadir    = "C:\\Users\\agocorona\\AppData\\Roaming\\cabal\\TCache-0.9.0.4"
libexecdir = "C:\\Users\\agocorona\\AppData\\Roaming\\cabal\\TCache-0.9.0.4"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "TCache_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "TCache_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "TCache_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "TCache_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)

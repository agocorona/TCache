module Paths_TCache (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,12,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\magocoal\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\magocoal\\AppData\\Roaming\\cabal\\i386-windows-ghc-7.6.3\\TCache-0.12.0"
datadir    = "C:\\Users\\magocoal\\AppData\\Roaming\\cabal\\i386-windows-ghc-7.6.3\\TCache-0.12.0"
libexecdir = "C:\\Users\\magocoal\\AppData\\Roaming\\cabal\\TCache-0.12.0"
sysconfdir = "C:\\Users\\magocoal\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "TCache_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "TCache_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "TCache_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "TCache_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "TCache_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)

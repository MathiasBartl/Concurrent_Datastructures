module Paths_concurrent_hashtable (
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
version = Version {versionBranch = [0,0,0,1], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/user/.cabal/bin"
libdir     = "/home/user/.cabal/lib/x86_64-linux-ghc-7.8.3/concurrent-hashtable-0.0.0.1"
datadir    = "/home/user/.cabal/share/x86_64-linux-ghc-7.8.3/concurrent-hashtable-0.0.0.1"
libexecdir = "/home/user/.cabal/libexec"
sysconfdir = "/home/user/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "concurrent_hashtable_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "concurrent_hashtable_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "concurrent_hashtable_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "concurrent_hashtable_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "concurrent_hashtable_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

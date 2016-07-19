module Paths_strip (
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
version = Version {versionBranch = [0,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/cprasenj/Library/Haskell/bin"
libdir     = "/Users/cprasenj/Library/Haskell/ghc-7.8.3-x86_64/lib/strip-0.0.0"
datadir    = "/Users/cprasenj/Library/Haskell/share/ghc-7.8.3-x86_64/strip-0.0.0"
libexecdir = "/Users/cprasenj/Library/Haskell/libexec"
sysconfdir = "/Users/cprasenj/Library/Haskell/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "strip_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "strip_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "strip_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "strip_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "strip_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

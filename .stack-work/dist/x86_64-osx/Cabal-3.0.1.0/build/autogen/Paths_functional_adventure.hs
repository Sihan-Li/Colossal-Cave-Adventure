{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_functional_adventure (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/lisihan/Documents/51400 Functional Programming/samli/functional-adventure/.stack-work/install/x86_64-osx/5abb25db37c2e59564af3b1f2ba19b3e9a03154e6fae75e5b357e6b8df26873c/8.8.3/bin"
libdir     = "/Users/lisihan/Documents/51400 Functional Programming/samli/functional-adventure/.stack-work/install/x86_64-osx/5abb25db37c2e59564af3b1f2ba19b3e9a03154e6fae75e5b357e6b8df26873c/8.8.3/lib/x86_64-osx-ghc-8.8.3/functional-adventure-0.1.0.0-FMBDJHnvJTKBXVd3S1y4Gu"
dynlibdir  = "/Users/lisihan/Documents/51400 Functional Programming/samli/functional-adventure/.stack-work/install/x86_64-osx/5abb25db37c2e59564af3b1f2ba19b3e9a03154e6fae75e5b357e6b8df26873c/8.8.3/lib/x86_64-osx-ghc-8.8.3"
datadir    = "/Users/lisihan/Documents/51400 Functional Programming/samli/functional-adventure/.stack-work/install/x86_64-osx/5abb25db37c2e59564af3b1f2ba19b3e9a03154e6fae75e5b357e6b8df26873c/8.8.3/share/x86_64-osx-ghc-8.8.3/functional-adventure-0.1.0.0"
libexecdir = "/Users/lisihan/Documents/51400 Functional Programming/samli/functional-adventure/.stack-work/install/x86_64-osx/5abb25db37c2e59564af3b1f2ba19b3e9a03154e6fae75e5b357e6b8df26873c/8.8.3/libexec/x86_64-osx-ghc-8.8.3/functional-adventure-0.1.0.0"
sysconfdir = "/Users/lisihan/Documents/51400 Functional Programming/samli/functional-adventure/.stack-work/install/x86_64-osx/5abb25db37c2e59564af3b1f2ba19b3e9a03154e6fae75e5b357e6b8df26873c/8.8.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "functional_adventure_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "functional_adventure_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "functional_adventure_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "functional_adventure_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "functional_adventure_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "functional_adventure_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

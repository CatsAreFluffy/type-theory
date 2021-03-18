{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_type_theory (
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

bindir     = "/Users/rohan/.cabal/bin"
libdir     = "/Users/rohan/.cabal/lib/x86_64-osx-ghc-8.10.4/type-theory-0.1.0.0-inplace-type-theory"
dynlibdir  = "/Users/rohan/.cabal/lib/x86_64-osx-ghc-8.10.4"
datadir    = "/Users/rohan/.cabal/share/x86_64-osx-ghc-8.10.4/type-theory-0.1.0.0"
libexecdir = "/Users/rohan/.cabal/libexec/x86_64-osx-ghc-8.10.4/type-theory-0.1.0.0"
sysconfdir = "/Users/rohan/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "type_theory_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "type_theory_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "type_theory_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "type_theory_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "type_theory_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "type_theory_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

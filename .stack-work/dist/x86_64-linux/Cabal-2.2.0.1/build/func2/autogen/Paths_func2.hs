{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_func2 (
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

bindir     = "/home/oskari/haskell2/func2/src/.stack-work/install/x86_64-linux/ae57c0e79bb6aea0ae3382d42fe449598460459c2329c5d3bd1c7b1b348bf673/8.4.4/bin"
libdir     = "/home/oskari/haskell2/func2/src/.stack-work/install/x86_64-linux/ae57c0e79bb6aea0ae3382d42fe449598460459c2329c5d3bd1c7b1b348bf673/8.4.4/lib/x86_64-linux-ghc-8.4.4/func2-0.1.0.0-GhI00fX7rAh4UZgmdEoj42-func2"
dynlibdir  = "/home/oskari/haskell2/func2/src/.stack-work/install/x86_64-linux/ae57c0e79bb6aea0ae3382d42fe449598460459c2329c5d3bd1c7b1b348bf673/8.4.4/lib/x86_64-linux-ghc-8.4.4"
datadir    = "/home/oskari/haskell2/func2/src/.stack-work/install/x86_64-linux/ae57c0e79bb6aea0ae3382d42fe449598460459c2329c5d3bd1c7b1b348bf673/8.4.4/share/x86_64-linux-ghc-8.4.4/func2-0.1.0.0"
libexecdir = "/home/oskari/haskell2/func2/src/.stack-work/install/x86_64-linux/ae57c0e79bb6aea0ae3382d42fe449598460459c2329c5d3bd1c7b1b348bf673/8.4.4/libexec/x86_64-linux-ghc-8.4.4/func2-0.1.0.0"
sysconfdir = "/home/oskari/haskell2/func2/src/.stack-work/install/x86_64-linux/ae57c0e79bb6aea0ae3382d42fe449598460459c2329c5d3bd1c7b1b348bf673/8.4.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "func2_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "func2_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "func2_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "func2_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "func2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "func2_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

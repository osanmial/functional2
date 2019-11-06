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

bindir     = "/home/oskari/haskell2/func2/.stack-work/install/x86_64-linux/bde5317b8b092ef724cde21f9de7ab44f0c694b1c06b9ffcad26298a32d18746/8.6.5/bin"
libdir     = "/home/oskari/haskell2/func2/.stack-work/install/x86_64-linux/bde5317b8b092ef724cde21f9de7ab44f0c694b1c06b9ffcad26298a32d18746/8.6.5/lib/x86_64-linux-ghc-8.6.5/func2-0.1.0.0-3cB5elwf4bkL7R6ZQxLoUJ-func2"
dynlibdir  = "/home/oskari/haskell2/func2/.stack-work/install/x86_64-linux/bde5317b8b092ef724cde21f9de7ab44f0c694b1c06b9ffcad26298a32d18746/8.6.5/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/oskari/haskell2/func2/.stack-work/install/x86_64-linux/bde5317b8b092ef724cde21f9de7ab44f0c694b1c06b9ffcad26298a32d18746/8.6.5/share/x86_64-linux-ghc-8.6.5/func2-0.1.0.0"
libexecdir = "/home/oskari/haskell2/func2/.stack-work/install/x86_64-linux/bde5317b8b092ef724cde21f9de7ab44f0c694b1c06b9ffcad26298a32d18746/8.6.5/libexec/x86_64-linux-ghc-8.6.5/func2-0.1.0.0"
sysconfdir = "/home/oskari/haskell2/func2/.stack-work/install/x86_64-linux/bde5317b8b092ef724cde21f9de7ab44f0c694b1c06b9ffcad26298a32d18746/8.6.5/etc"

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

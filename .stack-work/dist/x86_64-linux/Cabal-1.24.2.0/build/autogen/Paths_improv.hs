{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_improv (
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

bindir     = "/home/thomas/research_alli/improv/.stack-work/install/x86_64-linux/lts-8.5/8.0.2/bin"
libdir     = "/home/thomas/research_alli/improv/.stack-work/install/x86_64-linux/lts-8.5/8.0.2/lib/x86_64-linux-ghc-8.0.2/improv-0.1.0.0-SK2koE1cQA7mN0X0IxqKL"
dynlibdir  = "/home/thomas/research_alli/improv/.stack-work/install/x86_64-linux/lts-8.5/8.0.2/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/thomas/research_alli/improv/.stack-work/install/x86_64-linux/lts-8.5/8.0.2/share/x86_64-linux-ghc-8.0.2/improv-0.1.0.0"
libexecdir = "/home/thomas/research_alli/improv/.stack-work/install/x86_64-linux/lts-8.5/8.0.2/libexec"
sysconfdir = "/home/thomas/research_alli/improv/.stack-work/install/x86_64-linux/lts-8.5/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "improv_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "improv_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "improv_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "improv_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "improv_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "improv_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

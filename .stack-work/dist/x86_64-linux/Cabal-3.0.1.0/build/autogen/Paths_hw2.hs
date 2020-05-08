{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_hw2 (
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

bindir     = "/home/noname/Desktop/hw2-NonameUntitled/.stack-work/install/x86_64-linux/2e1da8e784bd65620f847fd9722d8a737ac3f18bd72751e7494048b7975feed7/8.8.3/bin"
libdir     = "/home/noname/Desktop/hw2-NonameUntitled/.stack-work/install/x86_64-linux/2e1da8e784bd65620f847fd9722d8a737ac3f18bd72751e7494048b7975feed7/8.8.3/lib/x86_64-linux-ghc-8.8.3/hw2-0.1.0.0-8GtEhd6VNCw8A53pGNDN5A"
dynlibdir  = "/home/noname/Desktop/hw2-NonameUntitled/.stack-work/install/x86_64-linux/2e1da8e784bd65620f847fd9722d8a737ac3f18bd72751e7494048b7975feed7/8.8.3/lib/x86_64-linux-ghc-8.8.3"
datadir    = "/home/noname/Desktop/hw2-NonameUntitled/.stack-work/install/x86_64-linux/2e1da8e784bd65620f847fd9722d8a737ac3f18bd72751e7494048b7975feed7/8.8.3/share/x86_64-linux-ghc-8.8.3/hw2-0.1.0.0"
libexecdir = "/home/noname/Desktop/hw2-NonameUntitled/.stack-work/install/x86_64-linux/2e1da8e784bd65620f847fd9722d8a737ac3f18bd72751e7494048b7975feed7/8.8.3/libexec/x86_64-linux-ghc-8.8.3/hw2-0.1.0.0"
sysconfdir = "/home/noname/Desktop/hw2-NonameUntitled/.stack-work/install/x86_64-linux/2e1da8e784bd65620f847fd9722d8a737ac3f18bd72751e7494048b7975feed7/8.8.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hw2_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hw2_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hw2_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hw2_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hw2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hw2_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

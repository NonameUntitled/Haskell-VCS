{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_fileSystem (
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

bindir     = "/home/noname/Desktop/hw2-NonameUntitled/.stack-work/install/x86_64-linux/731467968e292101d7b7ad35486a1128f2f11567ba6aa57a67b00a7e426df225/8.6.5/bin"
libdir     = "/home/noname/Desktop/hw2-NonameUntitled/.stack-work/install/x86_64-linux/731467968e292101d7b7ad35486a1128f2f11567ba6aa57a67b00a7e426df225/8.6.5/lib/x86_64-linux-ghc-8.6.5/fileSystem-0.1.0.0-5DY9L0y7DveLFIsqPlJ5rU"
dynlibdir  = "/home/noname/Desktop/hw2-NonameUntitled/.stack-work/install/x86_64-linux/731467968e292101d7b7ad35486a1128f2f11567ba6aa57a67b00a7e426df225/8.6.5/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/noname/Desktop/hw2-NonameUntitled/.stack-work/install/x86_64-linux/731467968e292101d7b7ad35486a1128f2f11567ba6aa57a67b00a7e426df225/8.6.5/share/x86_64-linux-ghc-8.6.5/fileSystem-0.1.0.0"
libexecdir = "/home/noname/Desktop/hw2-NonameUntitled/.stack-work/install/x86_64-linux/731467968e292101d7b7ad35486a1128f2f11567ba6aa57a67b00a7e426df225/8.6.5/libexec/x86_64-linux-ghc-8.6.5/fileSystem-0.1.0.0"
sysconfdir = "/home/noname/Desktop/hw2-NonameUntitled/.stack-work/install/x86_64-linux/731467968e292101d7b7ad35486a1128f2f11567ba6aa57a67b00a7e426df225/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "fileSystem_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "fileSystem_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "fileSystem_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "fileSystem_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "fileSystem_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "fileSystem_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

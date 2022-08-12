{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_deadlines (
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

bindir     = "/home/alexandre/Downloads/Paradigmas/deadlines/deadlines/.stack-work/install/x86_64-linux-tinfo6/4ccb3b50be70a8f5101048cc76337e9d338aa49a17b2a002ad2f5989567a4e85/9.0.2/bin"
libdir     = "/home/alexandre/Downloads/Paradigmas/deadlines/deadlines/.stack-work/install/x86_64-linux-tinfo6/4ccb3b50be70a8f5101048cc76337e9d338aa49a17b2a002ad2f5989567a4e85/9.0.2/lib/x86_64-linux-ghc-9.0.2/deadlines-0.1.0.0-HgA16jPEH72EylfJOOaurk"
dynlibdir  = "/home/alexandre/Downloads/Paradigmas/deadlines/deadlines/.stack-work/install/x86_64-linux-tinfo6/4ccb3b50be70a8f5101048cc76337e9d338aa49a17b2a002ad2f5989567a4e85/9.0.2/lib/x86_64-linux-ghc-9.0.2"
datadir    = "/home/alexandre/Downloads/Paradigmas/deadlines/deadlines/.stack-work/install/x86_64-linux-tinfo6/4ccb3b50be70a8f5101048cc76337e9d338aa49a17b2a002ad2f5989567a4e85/9.0.2/share/x86_64-linux-ghc-9.0.2/deadlines-0.1.0.0"
libexecdir = "/home/alexandre/Downloads/Paradigmas/deadlines/deadlines/.stack-work/install/x86_64-linux-tinfo6/4ccb3b50be70a8f5101048cc76337e9d338aa49a17b2a002ad2f5989567a4e85/9.0.2/libexec/x86_64-linux-ghc-9.0.2/deadlines-0.1.0.0"
sysconfdir = "/home/alexandre/Downloads/Paradigmas/deadlines/deadlines/.stack-work/install/x86_64-linux-tinfo6/4ccb3b50be70a8f5101048cc76337e9d338aa49a17b2a002ad2f5989567a4e85/9.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "deadlines_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "deadlines_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "deadlines_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "deadlines_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "deadlines_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "deadlines_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

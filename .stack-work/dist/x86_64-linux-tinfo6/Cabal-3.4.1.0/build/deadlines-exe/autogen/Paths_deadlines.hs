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

bindir     = "/home/tiago/repos/deadlines/.stack-work/install/x86_64-linux-tinfo6/696d679c19bbea3c193779d655897860186979a26a051daca876208e106f9c48/9.0.2/bin"
libdir     = "/home/tiago/repos/deadlines/.stack-work/install/x86_64-linux-tinfo6/696d679c19bbea3c193779d655897860186979a26a051daca876208e106f9c48/9.0.2/lib/x86_64-linux-ghc-9.0.2/deadlines-0.1.0.0-JEjl3VjFT2p9g2mx4Ue6HH-deadlines-exe"
dynlibdir  = "/home/tiago/repos/deadlines/.stack-work/install/x86_64-linux-tinfo6/696d679c19bbea3c193779d655897860186979a26a051daca876208e106f9c48/9.0.2/lib/x86_64-linux-ghc-9.0.2"
datadir    = "/home/tiago/repos/deadlines/.stack-work/install/x86_64-linux-tinfo6/696d679c19bbea3c193779d655897860186979a26a051daca876208e106f9c48/9.0.2/share/x86_64-linux-ghc-9.0.2/deadlines-0.1.0.0"
libexecdir = "/home/tiago/repos/deadlines/.stack-work/install/x86_64-linux-tinfo6/696d679c19bbea3c193779d655897860186979a26a051daca876208e106f9c48/9.0.2/libexec/x86_64-linux-ghc-9.0.2/deadlines-0.1.0.0"
sysconfdir = "/home/tiago/repos/deadlines/.stack-work/install/x86_64-linux-tinfo6/696d679c19bbea3c193779d655897860186979a26a051daca876208e106f9c48/9.0.2/etc"

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

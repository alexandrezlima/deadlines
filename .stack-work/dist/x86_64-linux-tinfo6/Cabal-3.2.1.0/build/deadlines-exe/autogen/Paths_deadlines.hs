{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
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

bindir     = "/home/tiago/repos/deadlines/deadlines/.stack-work/install/x86_64-linux-tinfo6/31da3134a8ff516d6ac54a895994810fdbefd3bfb993d4d0c24e1c3d4e2515d2/8.10.7/bin"
libdir     = "/home/tiago/repos/deadlines/deadlines/.stack-work/install/x86_64-linux-tinfo6/31da3134a8ff516d6ac54a895994810fdbefd3bfb993d4d0c24e1c3d4e2515d2/8.10.7/lib/x86_64-linux-ghc-8.10.7/deadlines-0.1.0.0-CaK2S05bEck7e8Jo7CigPu-deadlines-exe"
dynlibdir  = "/home/tiago/repos/deadlines/deadlines/.stack-work/install/x86_64-linux-tinfo6/31da3134a8ff516d6ac54a895994810fdbefd3bfb993d4d0c24e1c3d4e2515d2/8.10.7/lib/x86_64-linux-ghc-8.10.7"
datadir    = "/home/tiago/repos/deadlines/deadlines/.stack-work/install/x86_64-linux-tinfo6/31da3134a8ff516d6ac54a895994810fdbefd3bfb993d4d0c24e1c3d4e2515d2/8.10.7/share/x86_64-linux-ghc-8.10.7/deadlines-0.1.0.0"
libexecdir = "/home/tiago/repos/deadlines/deadlines/.stack-work/install/x86_64-linux-tinfo6/31da3134a8ff516d6ac54a895994810fdbefd3bfb993d4d0c24e1c3d4e2515d2/8.10.7/libexec/x86_64-linux-ghc-8.10.7/deadlines-0.1.0.0"
sysconfdir = "/home/tiago/repos/deadlines/deadlines/.stack-work/install/x86_64-linux-tinfo6/31da3134a8ff516d6ac54a895994810fdbefd3bfb993d4d0c24e1c3d4e2515d2/8.10.7/etc"

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

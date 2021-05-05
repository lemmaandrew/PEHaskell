{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_PEHaskell (
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

bindir     = "/home/peter/programs/ProjectEuler/PEHaskell/.stack-work/install/x86_64-linux-tinfo6/6c3ee2454757cfae6b135589e31de766475a4c49e6094857d3a8a2abc4da88f5/8.10.4/bin"
libdir     = "/home/peter/programs/ProjectEuler/PEHaskell/.stack-work/install/x86_64-linux-tinfo6/6c3ee2454757cfae6b135589e31de766475a4c49e6094857d3a8a2abc4da88f5/8.10.4/lib/x86_64-linux-ghc-8.10.4/PEHaskell-0.1.0.0-JBZx9bu1lcSBcZ0qBUIIn0-PEHaskell-bench"
dynlibdir  = "/home/peter/programs/ProjectEuler/PEHaskell/.stack-work/install/x86_64-linux-tinfo6/6c3ee2454757cfae6b135589e31de766475a4c49e6094857d3a8a2abc4da88f5/8.10.4/lib/x86_64-linux-ghc-8.10.4"
datadir    = "/home/peter/programs/ProjectEuler/PEHaskell/.stack-work/install/x86_64-linux-tinfo6/6c3ee2454757cfae6b135589e31de766475a4c49e6094857d3a8a2abc4da88f5/8.10.4/share/x86_64-linux-ghc-8.10.4/PEHaskell-0.1.0.0"
libexecdir = "/home/peter/programs/ProjectEuler/PEHaskell/.stack-work/install/x86_64-linux-tinfo6/6c3ee2454757cfae6b135589e31de766475a4c49e6094857d3a8a2abc4da88f5/8.10.4/libexec/x86_64-linux-ghc-8.10.4/PEHaskell-0.1.0.0"
sysconfdir = "/home/peter/programs/ProjectEuler/PEHaskell/.stack-work/install/x86_64-linux-tinfo6/6c3ee2454757cfae6b135589e31de766475a4c49e6094857d3a8a2abc4da88f5/8.10.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "PEHaskell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "PEHaskell_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "PEHaskell_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "PEHaskell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "PEHaskell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "PEHaskell_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

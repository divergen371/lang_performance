{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_benchmark (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/atsushi/.cabal/bin"
libdir     = "/Users/atsushi/.cabal/lib/aarch64-osx-ghc-9.4.8/benchmark-0.1.0.0-inplace-benchmark"
dynlibdir  = "/Users/atsushi/.cabal/lib/aarch64-osx-ghc-9.4.8"
datadir    = "/Users/atsushi/.cabal/share/aarch64-osx-ghc-9.4.8/benchmark-0.1.0.0"
libexecdir = "/Users/atsushi/.cabal/libexec/aarch64-osx-ghc-9.4.8/benchmark-0.1.0.0"
sysconfdir = "/Users/atsushi/.cabal/etc"

getBinDir     = catchIO (getEnv "benchmark_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "benchmark_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "benchmark_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "benchmark_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "benchmark_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "benchmark_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'

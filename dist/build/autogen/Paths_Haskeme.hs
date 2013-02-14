module Paths_Haskeme (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/arian/.cabal/bin"
libdir     = "/home/arian/.cabal/lib/Haskeme-0.1.0.0/ghc-7.4.2"
datadir    = "/home/arian/.cabal/share/Haskeme-0.1.0.0"
libexecdir = "/home/arian/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "Haskeme_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Haskeme_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Haskeme_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Haskeme_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

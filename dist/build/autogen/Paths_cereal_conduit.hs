module Paths_cereal_conduit (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,0,6], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "c:\\haskell\\bin"
libdir     = "c:\\haskell\\lib\\cereal-conduit-0.0.6\\ghc-7.4.1"
datadir    = "c:\\haskell\\data\\cereal-conduit-0.0.6"
libexecdir = "c:\\haskell\\cereal-conduit-0.0.6"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "cereal_conduit_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "cereal_conduit_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "cereal_conduit_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "cereal_conduit_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)

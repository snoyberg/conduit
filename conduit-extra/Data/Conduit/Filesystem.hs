{-# LANGUAGE RankNTypes #-}
-- | /NOTE/ It is recommended to start using "Data.Conduit.Combinators" instead
-- of this module.
module Data.Conduit.Filesystem
    ( sourceDirectory
    , sourceDirectoryDeep
    ) where

import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import Control.Monad.Trans.Resource (MonadResource)
import Control.Monad.IO.Class (liftIO)
import System.FilePath ((</>))

-- | Stream the contents of the given directory, without traversing deeply.
--
-- This function will return /all/ of the contents of the directory, whether
-- they be files, directories, etc.
--
-- Note that the generated filepaths will be the complete path, not just the
-- filename. In other words, if you have a directory @foo@ containing files
-- @bar@ and @baz@, and you use @sourceDirectory@ on @foo@, the results will be
-- @foo/bar@ and @foo/baz@.
--
-- Since 1.1.0
sourceDirectory :: MonadResource m => FilePath -> ConduitT i FilePath m ()
sourceDirectory = CC.sourceDirectory

-- | Deeply stream the contents of the given directory.
--
-- This works the same as @sourceDirectory@, but will not return directories at
-- all. This function also takes an extra parameter to indicate whether
-- symlinks will be followed.
--
-- Since 1.1.0
sourceDirectoryDeep :: MonadResource m
                    => Bool -- ^ Follow directory symlinks
                    -> FilePath -- ^ Root directory
                    -> ConduitT i FilePath m ()
sourceDirectoryDeep = CC.sourceDirectoryDeep

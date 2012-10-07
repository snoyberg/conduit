{-# OPTIONS_HADDOCK not-home #-}
module Data.Conduit.Network.TLS.Internal
    ( TLSConfig (..)
    ) where

import Prelude hiding (FilePath)
import Data.Conduit.Network (HostPreference)
import Filesystem.Path.CurrentOS (FilePath)

data TLSConfig = TLSConfig
    { tlsHost :: HostPreference
    , tlsPort :: Int
    , tlsCertificate :: FilePath
    , tlsKey :: FilePath
    , tlsNeedLocalAddr :: Bool
    }

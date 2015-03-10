{-# OPTIONS_HADDOCK not-home #-}
module Data.Conduit.Network.TLS.Internal
    ( TLSConfig (..)
    , TlsCertData (..)
    ) where

import Prelude hiding (FilePath)
import Data.Streaming.Network (HostPreference)
import qualified Data.ByteString as S

-- structure providing access to certificate and key data through call backs 
data TlsCertData = TlsCertData { getTLSCert :: IO S.ByteString
                               , getTLSChainCerts :: IO [S.ByteString]
                               , getTLSKey :: IO S.ByteString }


data TLSConfig = TLSConfig
    { tlsHost :: HostPreference
    , tlsPort :: Int
    , tlsCertData :: TlsCertData
    , tlsNeedLocalAddr :: Bool
    }

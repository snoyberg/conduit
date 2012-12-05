{-# LANGUAGE ScopedTypeVariables #-}
module Data.Conduit.Network.Utils
    ( -- * Helper utilities
      HostPreference (..)
    , bindPort
    , getSocket
    ) where

import Network.Socket (AddrInfo, Socket, SocketType)
import qualified Network.Socket as NS
import Data.String (IsString (fromString))
import Control.Exception (bracketOnError, IOException)
import qualified Control.Exception as E

-- | Attempt to connect to the given host/port using given @SocketType@.
getSocket :: String -> Int -> SocketType -> IO (Socket, AddrInfo)
getSocket host' port' sockettype = do
    let hints = NS.defaultHints {
                          NS.addrFlags = [NS.AI_ADDRCONFIG]
                        , NS.addrSocketType = sockettype
                        }
    (addr:_) <- NS.getAddrInfo (Just hints) (Just host') (Just $ show port')
    sock <- NS.socket (NS.addrFamily addr) (NS.addrSocketType addr)
                      (NS.addrProtocol addr)
    return (sock, addr)

-- | Which host to bind.
--
-- Note: The @IsString@ instance recognizes the following special values:
--
-- * @*@ means @HostAny@
--
-- * @*4@ means @HostIPv4@
--
-- * @*6@ means @HostIPv6@
data HostPreference =
    HostAny
  | HostIPv4
  | HostIPv6
  | Host String
    deriving (Eq, Ord, Show, Read)

instance IsString HostPreference where
    -- The funny code coming up is to get around some irritating warnings from
    -- GHC. I should be able to just write:
    {-
    fromString "*" = HostAny
    fromString "*4" = HostIPv4
    fromString "*6" = HostIPv6
    -}
    fromString s'@('*':s) =
        case s of
            [] -> HostAny
            ['4'] -> HostIPv4
            ['6'] -> HostIPv6
            _ -> Host s'
    fromString s = Host s

-- | Attempt to bind a listening @Socket@ on the given host/port using given
-- @SocketType@. If no host is given, will use the first address available.
bindPort :: Int -> HostPreference -> SocketType -> IO Socket
bindPort p s sockettype = do
    let hints = NS.defaultHints
            { NS.addrFlags = [ NS.AI_PASSIVE
                             , NS.AI_NUMERICSERV
                             , NS.AI_NUMERICHOST
                             ]
            , NS.addrSocketType = sockettype
            }
        host =
            case s of
                Host s' -> Just s'
                _ -> Nothing
        port = Just . show $ p
    addrs <- NS.getAddrInfo (Just hints) host port
    -- Choose an IPv6 socket if exists.  This ensures the socket can
    -- handle both IPv4 and IPv6 if v6only is false.
    let addrs4 = filter (\x -> NS.addrFamily x /= NS.AF_INET6) addrs
        addrs6 = filter (\x -> NS.addrFamily x == NS.AF_INET6) addrs
        addrs' =
            case s of
                HostIPv4 -> addrs4 ++ addrs6
                HostIPv6 -> addrs6 ++ addrs4
                _ -> addrs

        tryAddrs (addr1:rest@(_:_)) =
                                      E.catch
                                      (theBody addr1)
                                      (\(_ :: IOException) -> tryAddrs rest)
        tryAddrs (addr1:[])         = theBody addr1
        tryAddrs _                  = error "bindPort: addrs is empty"
        theBody addr =
          bracketOnError
          (NS.socket (NS.addrFamily addr) (NS.addrSocketType addr) (NS.addrProtocol addr))
          NS.sClose
          (\sock -> do
              NS.setSocketOption sock NS.ReuseAddr 1
              NS.setSocketOption sock NS.NoDelay 1
              NS.bindSocket sock (NS.addrAddress addr)
              return sock
          )
    tryAddrs addrs'

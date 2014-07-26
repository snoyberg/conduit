{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Conduit.Network.TCP
    ( -- ** Server 
      runGeneralTCPServer
      -- ** Client
    , runGeneralTCPClient
    ) where

import Control.Concurrent (forkIO)
import Control.Exception (bracket, bracketOnError, mask, finally)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Control (MonadBaseControl, control)
import Data.Streaming.Network (acceptSafe, bindPortTCP, getSocketFamilyTCP,
                                safeRecv)
import Data.Streaming.Network.Internal (AppData(..), ClientSettings(..),
                                        ServerSettings(..))
import Network.Socket (SockAddr, Socket, getSocketName, sClose)
import Network.Socket.ByteString (sendAll)

type ConnectionHandle m = Socket -> SockAddr -> Maybe SockAddr -> m ()

-- | Run a general TCP server on a handle
--
-- Same as 'runTCPServerWithHandle', except monad can be any instance of
-- 'MonadIO'.
--
-- Since 1.2.0

runGeneralTCPServerWithHandle :: (MonadIO m, MonadBaseControl IO m) =>
                               ServerSettings -> ConnectionHandle m -> m ()
runGeneralTCPServerWithHandle ((ServerSettings port host msocket afterBind needLocalAddr)) handle =
    case msocket of
        Nothing -> control $ \run -> bracket (bindPortTCP port host) sClose (run . inner)
        Just lsocket -> inner lsocket
  where
    inner lsocket = control $ \run -> afterBind lsocket >> forever (run $ serve lsocket)
    serve lsocket = control $ \run -> bracketOnError (acceptSafe lsocket) close (run . doit)
    close :: (Socket, SockAddr) -> IO ()
    close (socket, _) = sClose socket
    doit (socket, addr) = do
        mlocal <- if needLocalAddr
                    then fmap Just $ liftIO $ getSocketName socket
                    else return Nothing
        let app' run = run (handle socket addr mlocal) >> return ()
            appClose run = app' run `finally` sClose socket
        control $ \run ->
            mask $ \restore -> forkIO
                (restore $ appClose run) >> run (return())
        return ()

-- | Run a general TCP server
--
-- Same as 'runTCPServer', except monad can be any instance of 'MonadIO'.
--
-- Since 1.2.0

runGeneralTCPServer :: (MonadIO m, MonadBaseControl IO m) =>
                         ServerSettings -> (AppData -> m ()) -> m ()
runGeneralTCPServer settings app = runGeneralTCPServerWithHandle (settings) app'
  where app' socket addr mlocal =
          let ad = AppData
                    { appRead' = safeRecv socket 4096
                    , appWrite' = sendAll socket
                    , appSockAddr' = addr
                    , appLocalAddr' = mlocal
                    }
          in
            app ad
--
-- | Run a general TCP client
--
-- Same as 'runTCPClient', except monad can be any instance of 'MonadIO'.
--
-- Since 1.2.0

runGeneralTCPClient :: (MonadIO m, MonadBaseControl IO m) =>
                            ClientSettings -> (AppData -> m a) -> m a
runGeneralTCPClient (ClientSettings port host addrFamily) app =
    control $ \run -> bracket
        (getSocketFamilyTCP host port addrFamily)
        (sClose . fst)
        (\(s, address) -> run $ app AppData
            { appRead' = safeRecv s 4096
            , appWrite' = sendAll s
            , appSockAddr' = address
            , appLocalAddr' = Nothing
            })

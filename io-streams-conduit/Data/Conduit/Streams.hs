module Data.Conduit.Streams where

import qualified System.IO.Streams as Streams
import Data.Conduit
import Control.Monad.IO.Class

sourceStream :: MonadIO m
             => Streams.InputStream a
             -> Source m a
sourceStream os =
    loop
  where
    loop = liftIO (Streams.read os) >>= maybe (return ()) go
    go x = yield x >> loop

sinkStream :: MonadIO m
           => Streams.OutputStream a
           -> Sink a m ()
sinkStream is = awaitForever $ \x -> liftIO (Streams.write (Just x) is)

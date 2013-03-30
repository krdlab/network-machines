{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Network.Machines (
      source
    , sink
    , settings
    , AppData(..)
    , runServer
    ) where

import           Data.Machine               (SourceT, ProcessT)
import qualified Data.Machine               as M
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import           Network                    (PortID(..), PortNumber)
import           Network.Socket             (Socket)
import qualified Network                    as NS (listenOn, sClose)
import qualified Network.Socket             as NS (accept)
import qualified Network.Socket.ByteString  as NS
import           Control.Concurrent             (forkIO)
import           Control.Monad                  (forever, void)
import           Control.Monad.Trans.Control    (MonadBaseControl, control)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Exception          (bracket, finally)

source :: MonadIO m => Socket -> SourceT m ByteString
source sock = M.repeatedly $ do
    bs <- liftIO $ NS.recv sock 4096
    if BS.null bs
        then M.stop
        else M.yield bs

sink :: MonadIO m => Socket -> ProcessT m ByteString ()
sink sock = M.repeatedly $ do
    i <- M.await
    liftIO $ NS.sendAll sock i

data Settings = Settings
    { listenPort :: PortID
    }
    deriving (Show)

settings :: PortNumber -> Settings
settings p = Settings
    { listenPort = PortNumber p
    }

data AppData m = AppData
    { appSource :: SourceT m ByteString
    , appSink   :: ProcessT m ByteString ()
    }

type Application m = AppData m -> m ()

runServer :: (MonadIO m, MonadBaseControl IO m) => Settings -> Application m -> m ()
runServer s app = control $ \run -> bracket
    (NS.listenOn $ listenPort s)
    NS.sClose
    (run . forever . serve)
    where
        serve lsock = do
            (sock, _) <- liftIO $ NS.accept lsock  -- liftIO :: IO a -> m a
            let ad = AppData { appSource = source sock
                             , appSink   = sink sock
                             }
                runApp r = void $ r (app ad)                  -- (m () ->  f a) ->  f ()
                closeSock = NS.sClose sock
                app' r = runApp r `finally` closeSock    -- (m () -> IO a) -> IO ()
            control $ \run -> forkIO (app' run) >> run (return ()) -- (RunBase m IO -> IO (StM m ())) -> m ()


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Network.Machine (
      source
    , sink
    , settings
    , runServer
    ) where

import           Data.Machine                   (SourceT, ProcessT)
import qualified Data.Machine                   as M
import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as BS
import           Network                        (PortID(..), PortNumber)
import           Network.Socket                 (Socket)
import qualified Network                        as NS (listenOn, sClose)
import qualified Network.Socket                 as NS (accept)
import qualified Network.Socket.ByteString      as NS
import           Control.Concurrent             (forkIO)
import           Control.Monad                  (forever, void)
import           Control.Monad.Trans.Control    (MonadBaseControl, control)
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Control.Exception              (bracket, finally)
import           Network.Machine.Application   (Application)
import qualified Network.Machine.Application   as App

source :: MonadIO m => Socket -> SourceT m ByteString
source sock = M.repeatedly $ do
    bs <- liftIO $ NS.recv sock 10
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

runServer :: (MonadIO m, MonadBaseControl IO m) => Settings -> Application m -> m ()
runServer s app = control $ \run -> bracket
    (NS.listenOn $ listenPort s)  -- TODO: use listen
    NS.sClose
    (run . forever . serve)
    where
        serve lsock = do
            (sock, _) <- liftIO $ NS.accept lsock
            let ctx = App.Context
                    { App.source = source sock
                    , App.sink   = sink   sock
                    }
                runApp r  = void $ r $ app ctx
                closeSock = NS.sClose sock
            control $ \run -> forkIO (runApp run `finally` closeSock) >> run (return ())


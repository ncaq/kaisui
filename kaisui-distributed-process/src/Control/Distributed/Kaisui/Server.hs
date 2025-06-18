module Control.Distributed.Kaisui.Server
  ( runServer
  ) where

import Control.Distributed.Kaisui.Transport
import Control.Distributed.Kaisui.Types
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Data.Convertible
import RIO

-- | Run a distributed process server
runServer :: (HasLogFunc env) => Text -> Text -> RIO env ()
runServer host port = do
  logInfo $ "Starting distributed server on " <> display host <> ":" <> display port
  nodeResult <- liftIO $ createNode (convert host) (convert port)
  case nodeResult of
    Left err -> logError $ "Failed to create node: " <> displayShow err
    Right node -> do
      liftIO $ runProcess node runServerProcess
      liftIO $ closeNodeSafely node

-- | Server process implementation
runServerProcess :: Process ()
runServerProcess = do
  self <- getSelfPid
  register "text_server" self
  say $ "Server process started with PID: " ++ show self
  say "Server registered as 'text_server'"
  say "Server waiting for text messages..."
  serverLoop

-- | Main server loop that handles incoming messages
serverLoop :: Process ()
serverLoop = do
  receiveWait
    [ match $ \(sender, TextMessage msg) -> do
        say $ "Server received from " ++ show sender ++ ": " ++ convert msg
        let response = TextMessage ("Echo: " <> msg)
        send sender response
        say $ "Server sent response: " ++ convert ("Echo: " <> msg)
        serverLoop
    ]

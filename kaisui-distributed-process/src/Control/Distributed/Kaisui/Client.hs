module Control.Distributed.Kaisui.Client
  ( runClient
  ) where

import Control.Distributed.Kaisui.Transport
import Control.Distributed.Kaisui.Types
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Data.Convertible
import Network.Transport (EndPointAddress (..))
import RIO

-- | Run a distributed process client
runClient :: (HasLogFunc env) => Text -> Text -> Text -> RIO env ()
runClient host port message = do
  logInfo $ "Connecting to distributed server at " <> display host <> ":" <> display port
  nodeResult <- liftIO $ createNode "127.0.0.1" "0" -- Use dynamic port for client
  case nodeResult of
    Left err -> logError $ "Failed to create node: " <> displayShow err
    Right node -> do
      liftIO $ runProcess node (runClientProcess host port message)
      liftIO $ closeNodeSafely node

-- | Client process implementation
runClientProcess :: Text -> Text -> Text -> Process ()
runClientProcess host port message = do
  self <- getSelfPid
  say $ "Client process started with PID: " ++ show self

  -- Create server node identifier for connection
  let addressText = host <> ":" <> port <> ":0"
      serverAddr = EndPointAddress $ convert addressText
      serverNodeId = NodeId serverAddr

  -- Try to connect to the remote server
  say $ "Attempting to connect to server at " ++ convert host ++ ":" ++ convert port

  -- Use whereisRemoteAsync to find the server process
  whereisRemoteAsync serverNodeId "text_server"

  -- Wait for the server process lookup result
  lookupResult <- expectTimeout 5000000 -- 5 second timeout
  case lookupResult of
    Nothing -> do
      say "Timeout: Could not find server within 5 seconds"
    Just (WhereIsReply _ (Just serverPid)) -> do
      say $ "Found server process: " ++ show serverPid
      say $ "Sending message: " ++ convert message

      -- Send message to the remote server
      send serverPid (self, TextMessage message)

      -- Wait for response from server
      responseResult <- expectTimeout 3000000 -- 3 second timeout
      case responseResult of
        Nothing -> do
          say "Timeout: No response from server"
        Just (TextMessage responseText) -> do
          say $ "Client received response: " ++ convert responseText
    Just (WhereIsReply _ Nothing) -> do
      say "Server process 'text_server' not found on remote node"

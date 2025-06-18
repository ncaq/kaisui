module Control.Distributed.Kaisui.Client
  ( runClient
  ) where

import Control.Distributed.Kaisui.Transport
import Control.Distributed.Kaisui.Types
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Data.Binary
import Data.Convertible
import Network.Transport (EndPointAddress (..))
import RIO
import qualified RIO.ByteString.Lazy as LBS
import qualified RIO.Text as T
import Text.Printf

-- | Run a distributed process client
runClient :: (HasLogFunc env) => Text -> Text -> Text -> RIO env ()
runClient host port message = do
  logInfo $ "Connecting to distributed server at " <> display host <> ":" <> display port
  nodeResult <- liftIO $ createNode "127.0.0.1" "0" -- Use dynamic port for client
  case nodeResult of
    Left err -> logError $ "Failed to create node: " <> displayShow err
    Right node -> liftIO $ do
      runProcess node (runClientProcess host port message)
      closeNodeSafely node

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

      -- Verbose logging for sending message
      let sendMsg = TextMessage message
          sendTuple = (self, sendMsg)
          msgBinary = encode sendMsg
          msgBinaryHex = T.intercalate " " $ map (\w -> convert (printf "%02x" w :: String)) (convert msgBinary :: [Word8])
          tupleBinary = encode sendTuple
          tupleBinaryHex = T.intercalate " " $ map (\w -> convert (printf "%02x" w :: String)) (convert tupleBinary :: [Word8])

      say "=== CLIENT SENDING MESSAGE ==="
      say $ "To server PID: " ++ show serverPid
      say $ "Client PID: " ++ show self
      say $ "Message content: " ++ convert message
      say "Message type: TextMessage"
      say $ "Message binary (hex): " ++ convert msgBinaryHex
      say $ "Message binary length: " ++ show (LBS.length msgBinary) ++ " bytes"
      say $ "Message show: " ++ show sendMsg
      say $ "Full tuple binary (hex): " ++ convert tupleBinaryHex
      say $ "Full tuple binary length: " ++ show (LBS.length tupleBinary) ++ " bytes"
      say $ "Full tuple show: " ++ show sendTuple

      -- Send message to the remote server
      send serverPid sendTuple

      -- Wait for response from server with verbose logging
      say "=== CLIENT WAITING FOR RESPONSE ==="
      say "Timeout: 3 seconds"
      responseResult <- expectTimeout 3000000 -- 3 second timeout
      case responseResult of
        Nothing -> do
          say "=== CLIENT TIMEOUT ==="
          say "ERROR: No response from server within timeout period"
          say "Expected: TextMessage response"
          say "Received: Nothing (timeout)"
        Just (TextMessage responseText) -> do
          let responseMsg = TextMessage responseText
              responseBinary = encode responseMsg
              responseBinaryHex = T.intercalate " " $ map (\w -> convert (printf "%02x" w :: String)) (convert responseBinary :: [Word8])

          say "=== CLIENT RECEIVED RESPONSE ==="
          say $ "Response content: " ++ convert responseText
          say "Response type: TextMessage"
          say $ "Response binary (hex): " ++ convert responseBinaryHex
          say $ "Response binary length: " ++ show (LBS.length responseBinary) ++ " bytes"
          say $ "Response show: " ++ show responseMsg
          say "=== CLIENT-SERVER COMMUNICATION COMPLETED ==="
    Just (WhereIsReply _ Nothing) -> do
      say "Server process 'text_server' not found on remote node"

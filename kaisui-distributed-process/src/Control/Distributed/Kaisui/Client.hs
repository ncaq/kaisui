module Control.Distributed.Kaisui.Client
  ( runClient
  ) where

import Control.Distributed.Kaisui.TextMessage
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Data.Binary
import Data.Convertible
import Network.Transport (EndPointAddress (..))
import Network.Transport.TCP
import RIO
import qualified RIO.ByteString as BS
import qualified RIO.ByteString.Lazy as LBS
import qualified RIO.Text as T
import Text.Printf

-- | Run a distributed process client
runClient :: (HasLogFunc env) => Text -> Text -> Text -> RIO env ()
runClient host port message = do
  logInfo $ "Connecting to distributed server at " <> display host <> ":" <> display port
  nodeResult <- liftIO $ createTransport (defaultTCPAddr "127.0.0.1" "0") defaultTCPParameters -- Use dynamic port for client
  case nodeResult of
    Left err -> logError $ "Failed to create node: " <> displayShow err
    Right transport -> liftIO $ do
      node <- newLocalNode transport initRemoteTable
      runProcess node (runClientProcess host port message)
      closeLocalNode node

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
          msgBinary = convert message
          msgBinaryBytes = BS.unpack msgBinary
          msgBinaryHex = T.intercalate " " $ map (\w -> convert (printf "%02x" w :: String)) msgBinaryBytes
          sendTuple = (self, msgBinary)
          tupleBinary = encode sendTuple
          tupleBinaryBytes = convert tupleBinary :: [Word8]
          tupleBinaryHex = T.intercalate " " $ map (\w -> convert (printf "%02x" w :: String)) tupleBinaryBytes

      say "=== CLIENT SENDING MESSAGE ==="
      say $ "To server PID: " ++ show serverPid
      say $ "Client PID: " ++ show self
      say $ "Message content: " ++ convert message
      say "Message type: TextMessage"
      say $ "Message binary (hex): " ++ convert msgBinaryHex
      say $ "Message binary array: " ++ show msgBinaryBytes
      say $ "Message binary length: " ++ show (BS.length msgBinary) ++ " bytes"
      say $ "Message show: " ++ show sendMsg
      say $ "Full tuple binary (hex): " ++ convert tupleBinaryHex
      say $ "Full tuple binary array: " ++ show tupleBinaryBytes
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
        Just (responseBinary :: ByteString) -> do
          let responseText = convert responseBinary
              responseMsg = TextMessage responseText
              responseBinaryBytes = BS.unpack responseBinary
              responseBinaryHex = T.intercalate " " $ map (\w -> convert (printf "%02x" w :: String)) responseBinaryBytes

          say "=== CLIENT RECEIVED RESPONSE ==="
          say $ "Response content: " ++ convert responseText
          say "Response type: TextMessage"
          say $ "Response binary (hex): " ++ convert responseBinaryHex
          say $ "Response binary array: " ++ show responseBinaryBytes
          say $ "Response binary length: " ++ show (BS.length responseBinary) ++ " bytes"
          say $ "Response show: " ++ show responseMsg
          say "=== CLIENT-SERVER COMMUNICATION COMPLETED ==="
    Just (WhereIsReply _ Nothing) -> say "Server process 'text_server' not found on remote node"

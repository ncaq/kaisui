module Control.Distributed.Kaisui.Server
  ( runServer
  ) where

import Control.Distributed.Kaisui.TextMessage
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Data.Convertible
import Network.Transport.TCP
import RIO
import qualified RIO.ByteString as BS
import qualified RIO.Text as T
import Text.Printf

-- | Run a distributed process server
runServer :: (HasLogFunc env) => Text -> Text -> RIO env ()
runServer host port = do
  logInfo $ "Starting distributed server on " <> display host <> ":" <> display port
  nodeResult <- liftIO $ createTransport (defaultTCPAddr (convert host) (convert port)) defaultTCPParameters
  case nodeResult of
    Left err -> logError $ "Failed to create node: " <> displayShow err
    Right transport -> liftIO $ do
      node <- newLocalNode transport initRemoteTable
      runProcess node runServerProcess
      closeLocalNode node

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
  say "=== SERVER WAITING FOR MESSAGES ==="
  receiveWait
    [ match $ \(sender, bs :: ByteString) -> handleClientMessage sender bs >> serverLoop
    ]

-- | Handle a single message from a client
handleClientMessage :: ProcessId -> ByteString -> Process ()
handleClientMessage sender bs = do
  case decodeProtobuf bs of
    Left err -> logDecodeError bs err
    Right receivedMsg -> processValidMessage sender bs receivedMsg

-- | Log protocol buffer decode error
logDecodeError :: ByteString -> String -> Process ()
logDecodeError bs err = do
  say "=== ERROR DECODING PROTOCOL BUFFER ==="
  say $ "Error: " ++ err
  say $ "Binary (hex): " ++ convert (bytesToHex bs)

-- | Process a successfully decoded message
processValidMessage :: ProcessId -> ByteString -> TextMessage -> Process ()
processValidMessage sender bs receivedMsg = do
  let msg = receivedMsg ^. body

  -- Log received message
  logReceivedMessage sender bs msg receivedMsg

  -- Create and send response
  let response = TextMessage ("Echo: " <> msg)
      responseBytes = encodeProtobuf response

  -- Log response
  logSendingResponse sender msg response responseBytes

  -- Send response
  send sender responseBytes
  say "=== MESSAGE EXCHANGE COMPLETED ==="

-- | Log details about a received message
logReceivedMessage :: ProcessId -> ByteString -> Text -> TextMessage -> Process ()
logReceivedMessage sender bs msg receivedMsg = do
  say "=== SERVER RECEIVED MESSAGE ==="
  say $ "From PID: " ++ show sender
  say $ "Message content: " ++ convert msg
  say "Message type: TextMessage"
  say $ "Binary representation (hex): " ++ convert (bytesToHex bs)
  say $ "Binary length: " ++ show (BS.length bs) ++ " bytes"
  say $ "Message show: " ++ show receivedMsg

-- | Log details about a response being sent
logSendingResponse :: ProcessId -> Text -> TextMessage -> ByteString -> Process ()
logSendingResponse sender msg response responseBytes = do
  say "=== SERVER SENDING RESPONSE ==="
  say $ "To PID: " ++ show sender
  say $ "Response content: " ++ convert ("Echo: " <> msg)
  say "Response type: TextMessage (Protocol Buffer)"
  say $ "Response binary (hex): " ++ convert (bytesToHex responseBytes)
  say $ "Response binary length: " ++ show (BS.length responseBytes) ++ " bytes"
  say $ "Response show: " ++ show response

-- | Convert ByteString to hex string representation
bytesToHex :: ByteString -> Text
bytesToHex bs = T.intercalate " " $ map (\w -> convert (printf "%02x" w :: String)) (BS.unpack bs)

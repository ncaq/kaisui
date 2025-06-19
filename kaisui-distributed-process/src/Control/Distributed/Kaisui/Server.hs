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
    [ match $ \(sender, bs :: ByteString) -> do
        -- Decode from Protocol Buffers format
        -- Verbose logging for received message
        let msg = convert bs
            receivedMsg = TextMessage msg
            msgBinaryHex = T.intercalate " " $ map (\w -> convert (printf "%02x" w :: String)) (BS.unpack bs)

        say "=== SERVER RECEIVED MESSAGE ==="
        say $ "From PID: " ++ show sender
        say $ "Message content: " ++ convert msg
        say "Message type: TextMessage"
        say $ "Binary representation (hex): " ++ convert msgBinaryHex
        say $ "Binary length: " ++ show (BS.length bs) ++ " bytes"
        say $ "Message show: " ++ show receivedMsg

        -- Create and send response with verbose logging
        let response = TextMessage ("Echo: " <> msg)
            responseBody = response ^. body
            responseBinaryHex =
              T.intercalate " "
                $ map
                  (\w -> convert (printf "%02x" w :: String))
                  (BS.unpack $ convert responseBody)

        say "=== SERVER SENDING RESPONSE ==="
        say $ "To PID: " ++ show sender
        say $ "Response content: " ++ convert ("Echo: " <> msg)
        say "Response type: TextMessage"
        say $ "Response binary (hex): " ++ convert responseBinaryHex
        say $ "Response binary length: " ++ show (T.length responseBody) ++ " bytes"
        say $ "Response show: " ++ show response

        send sender responseBody
        say "=== MESSAGE EXCHANGE COMPLETED ==="
        serverLoop
    ]

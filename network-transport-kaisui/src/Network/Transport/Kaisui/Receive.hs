module Network.Transport.Kaisui.Receive
  ( connectionReceiveLoop
  ) where

import qualified Network.Socket.ByteString as NSB
import qualified Network.Transport as NT
import Network.Transport.Kaisui.Connection
import Network.Transport.Kaisui.Error
import Network.Transport.Kaisui.Protocol
import Network.Transport.Kaisui.Type.Connection
import Network.Transport.Kaisui.Type.EndPoint
import qualified Proto.Kaisui.DataMessage as P
import Proto.Kaisui.Envelope
import RIO
import qualified RIO.ByteString as BS
import qualified RIO.HashMap as HM

-- | Connection receive loop
connectionReceiveLoop
  :: (HasLogFunc env, MonadReader env m, MonadThrow m, MonadUnliftIO m) => KaisuiEndPoint -> KaisuiConnection -> m ()
connectionReceiveLoop ep conn = handleLoop `catch` (\e -> handleException (e :: SomeException))
 where
  handleLoop = forever $ do
    bs <- liftIO $ NSB.recv (conn ^. socket) 4096
    if BS.null bs
      then handleConnectionClosed
      else handleReceivedData bs
  handleException e = do
    -- Log the error unless it's an expected connection close
    logDebug $ "Connection receive error: " <> displayShow e
    cleanupConnection
    throwM e -- Re-throw to properly exit the thread
  connId = conn ^. connectionId
  -- Handle connection closed by peer
  handleConnectionClosed = do
    logDebug $ "Connection closed by peer: " <> displayShow connId
    cleanupConnection
  -- Handle received data
  handleReceivedData bs =
    case decodeEnvelope bs of
      Left err -> do
        -- Log decoding error and close connection (protocol is corrupted)
        logWarn $ "Failed to decode envelope on connection " <> displayShow connId <> ": " <> displayShow err
        throwDecodingFailed "Failed to decode envelope" err
      Right envelope -> handleEnvelope envelope
  -- Handle decoded envelope
  handleEnvelope :: (MonadIO m, MonadThrow m) => Envelope -> m ()
  handleEnvelope envelope =
    case envelope ^. message of
      Just (DataMessageMessage msg) ->
        atomically
          $ writeTBQueue (ep ^. receiveQueue)
          $ NT.Received connId [msg ^. P.payload]
      Just (CloseConnectionMessage _) -> do
        cleanupConnection
        exitSuccess
      Just other ->
        -- Unexpected message type in data stream, protocol error
        throwUnexpectedMessage $ "Unexpected message type in data stream: " <> utf8BuilderToText (displayShow other)
      Nothing ->
        -- Empty envelope, protocol error
        throwUnexpectedMessage "Empty envelope received"
  -- Clean up connection
  cleanupConnection :: (MonadIO m) => m ()
  cleanupConnection = do
    atomically $ do
      modifyTVar (ep ^. connections) (HM.delete connId)
      writeTBQueue (ep ^. receiveQueue) $ NT.ConnectionClosed connId
    closeKaisuiConnection conn

module Network.Transport.Kaisui.Connection
  ( newKaisuiConnection
  , closeKaisuiConnection
  , sendOnKaisuiConnection
  ) where

import Network.Socket
import qualified Network.Socket.ByteString as NSB
import qualified Network.Transport as NT
import Network.Transport.Kaisui.Type.Connection as CT
import Network.Transport.Kaisui.Type.ConnectionState
import RIO

-- | Create a new Kaisui connection
newKaisuiConnection :: NT.ConnectionId -> NT.Reliability -> Socket -> SockAddr -> STM KaisuiConnection
newKaisuiConnection cid rel sock addr = do
  st <- newTVar ConnectionEstablished
  pure
    KaisuiConnection
      { CT.connectionId = cid
      , CT.reliability = rel
      , CT.socket = sock
      , CT.remoteAddr = addr
      , CT.state = st
      }

-- | Close a connection
closeKaisuiConnection :: (MonadIO m) => KaisuiConnection -> m ()
closeKaisuiConnection conn = do
  -- Update state first
  wasOpen <- atomically $ do
    oldState <- readTVar (conn ^. CT.state)
    writeTVar (conn ^. CT.state) ConnectionClosed
    pure $ oldState == ConnectionEstablished
  -- Close socket if it was open
  when wasOpen $ liftIO $ close (conn ^. CT.socket)

-- | Send data on a connection
sendOnKaisuiConnection :: (MonadIO m, MonadThrow m) => KaisuiConnection -> ByteString -> m ()
sendOnKaisuiConnection conn bs = do
  -- Check state atomically to avoid race conditions with closeKaisuiConnection
  canSend <- atomically $ do
    st <- readTVar (conn ^. CT.state)
    pure $ st == ConnectionEstablished
  if canSend
    then liftIO $ NSB.sendAll (conn ^. CT.socket) bs
    else throwM $ NT.TransportError NT.SendClosed "Connection is closed"

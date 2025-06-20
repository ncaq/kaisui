module Network.Transport.Kaisui.EndPoint
  ( createKaisuiEndPoint
  , closeKaisuiEndPoint
  , receiveEvent
  ) where

import qualified Control.Concurrent.STM.TBQueue as STM
import qualified Network.Socket as NS
import qualified Network.Transport as NT
import Network.Transport.Kaisui.Connection
import Network.Transport.Kaisui.Type.EndPoint as EP
import RIO
import qualified RIO.HashMap as HM

-- | Create a new endpoint
createKaisuiEndPoint :: NT.EndPointAddress -> NS.Socket -> Int -> STM KaisuiEndPoint
createKaisuiEndPoint addr sock queueSize =
  KaisuiEndPoint addr sock
    <$> newTVar HM.empty
    <*> newTBQueue (fromIntegral queueSize)
    <*> newTVar False

-- | Close an endpoint
closeKaisuiEndPoint :: (MonadIO m) => KaisuiEndPoint -> m ()
closeKaisuiEndPoint ep = do
  -- Mark as closed
  atomically $ writeTVar (ep ^. EP.closed) True
  -- Close all connections
  connsMap <- readTVarIO (ep ^. EP.connections)
  mapM_ closeKaisuiConnection (HM.elems connsMap)
  -- Close the socket
  liftIO $ NS.close (ep ^. EP.socket)

-- | Receive an event from the endpoint
receiveEvent :: (MonadIO m) => KaisuiEndPoint -> m NT.Event
receiveEvent ep = atomically $ STM.readTBQueue (ep ^. EP.receiveQueue)

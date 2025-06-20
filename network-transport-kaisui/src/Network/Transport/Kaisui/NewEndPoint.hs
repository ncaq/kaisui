module Network.Transport.Kaisui.NewEndPoint
  ( newKaisuiEndPoint
  ) where

import Data.Convertible
import Network.Socket
import Network.Transport as NT
import Network.Transport.Kaisui.Accept
import Network.Transport.Kaisui.Adapter
import Network.Transport.Kaisui.Connect
import Network.Transport.Kaisui.EndPoint
import Network.Transport.Kaisui.Type.Transport as KT
import RIO
import qualified RIO.HashMap as HM
import qualified RIO.Text as T

-- | Create new endpoint
-- Note: Returns Either to conform to Network.Transport's Transport interface,
-- which requires: IO (Either (TransportError NewEndPointErrorCode) EndPoint)
-- MonadThrow is used internally for error handling in sub-components
newKaisuiEndPoint
  :: (HasLogFunc env, MonadReader env m, MonadThrow m, MonadUnliftIO m)
  => KaisuiTransport -> m (Either (NT.TransportError NT.NewEndPointErrorCode) EndPoint)
newKaisuiEndPoint kt = do
  runIO <- askRunInIO
  -- Generate unique endpoint address
  endPointAddr <- atomically $ do
    currentCount <- readTVar (kt ^. KT.counter)
    writeTVar (kt ^. KT.counter) (currentCount + 1)
    let addr = kt ^. KT.host <> ":" <> kt ^. KT.port <> ":" <> T.pack (show currentCount)
    pure $ EndPointAddress $ convert addr
  -- Create socket for endpoint
  socketCreationResult <- try $ liftIO $ do
    let hints = defaultHints{addrFlags = [AI_PASSIVE], addrSocketType = Stream}
    addr : _ <- getAddrInfo (Just hints) (Just $ convert $ kt ^. KT.host) (Just $ convert $ kt ^. KT.port)
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    setSocketOption sock ReuseAddr 1
    bind sock (addrAddress addr)
    listen sock 128
    pure (sock, addr)
  case socketCreationResult of
    Left (e :: SomeException) ->
      pure $ Left $ NT.TransportError NT.NewEndPointFailed (displayException e)
    Right (sock, _) -> do
      -- Create endpoint
      ep <- atomically $ do
        endpoint <- createKaisuiEndPoint endPointAddr sock 1000
        modifyTVar (kt ^. KT.endPoints) (HM.insert endPointAddr endpoint)
        pure endpoint
      -- Start accept loop
      acceptAsync <- async $ acceptLoop ep
      link acceptAsync
      pure $ Right $ toEndPoint ep (\e a r h -> runIO $ connectKaisui e a r h)

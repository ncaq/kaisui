module Network.Transport.Kaisui.Transport
  ( createTransport
  ) where

import Data.Convertible
import qualified Network.Socket as NS
import Network.Transport
import Network.Transport.Kaisui.Adapter
import Network.Transport.Kaisui.EndPoint
import Network.Transport.Kaisui.NewEndPoint
import Network.Transport.Kaisui.Type.Transport
import RIO
import qualified RIO.HashMap as HM

-- | Create a new Kaisui transport for bridging with Rust
createTransport :: (MonadIO m) => Text -> Text -> m Transport
createTransport h p = do
  kt <- createKaisuiTransport h p
  pure $ toTransport kt (runSimpleApp . newKaisuiEndPoint) closeKaisuiTransport

-- | Create internal Kaisui transport
createKaisuiTransport :: (MonadIO m) => Text -> Text -> m KaisuiTransport
createKaisuiTransport h p = do
  -- Test that we can bind to the address
  liftIO $ do
    addr <- resolve
    sock <- testSocket addr
    NS.close sock

  KaisuiTransport
    <$> newTVarIO HM.empty
    <*> newTVarIO 0
    <*> pure h
    <*> pure p
 where
  resolve = do
    let hints = NS.defaultHints{NS.addrFlags = [NS.AI_PASSIVE], NS.addrSocketType = NS.Stream}
    addr : _ <- NS.getAddrInfo (Just hints) (Just $ convert h) (Just $ convert p)
    pure addr

  testSocket addr = do
    sock <- NS.socket (NS.addrFamily addr) (NS.addrSocketType addr) (NS.addrProtocol addr)
    NS.setSocketOption sock NS.ReuseAddr 1
    NS.bind sock (NS.addrAddress addr)
    pure sock

-- | Close the transport
closeKaisuiTransport :: (MonadIO m) => KaisuiTransport -> m ()
closeKaisuiTransport transport = do
  eps <- readTVarIO (transport ^. endPoints)
  mapM_ closeKaisuiEndPoint (HM.elems eps)

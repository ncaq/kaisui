module Network.Transport.Kaisui.CreateTransport
  ( createTransport
  ) where

import Network.Transport
import Network.Transport.Kaisui.Addr
import Network.Transport.Kaisui.Parameters
import RIO

-- | Create a Kaisui transport.
createTransport :: (MonadIO m, MonadThrow m) => KaisuiAddr -> KaisuiParameters -> m Transport
createTransport = undefined

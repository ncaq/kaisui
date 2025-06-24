module Network.Transport.Kaisui.Addr
  ( KaisuiAddr
  , defaultKaisuiAddr
  ) where

import Control.Lens (makeFieldsId)
import Network.Socket
import RIO

-- | Addressability of a transport.
data KaisuiAddr

makeFieldsId ''KaisuiAddr

-- | The bind and external host/port are the same.
defaultKaisuiAddr :: HostName -> ServiceName -> KaisuiAddr
defaultKaisuiAddr = undefined

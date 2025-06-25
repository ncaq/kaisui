module Network.Transport.Kaisui.Addr
  ( KaisuiAddr (..)
  , defaultKaisuiAddr
  ) where

import Network.Socket
import Network.Transport.Kaisui.AddrInfo
import RIO

-- | Addressability of a transport.
data KaisuiAddr
  = Addressable KaisuiAddrInfo
  | Unaddressable
  deriving (Generic)

-- | The bind and external host/port are the same.
defaultKaisuiAddr :: HostName -> ServiceName -> KaisuiAddr
defaultKaisuiAddr host port =
  Addressable
    $ KaisuiAddrInfo
      { bindHost = host
      , bindPort = port
      , externalAddress = (,) host
      }

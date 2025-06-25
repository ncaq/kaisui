module Network.Transport.Kaisui.AddrInfo
  ( KaisuiAddrInfo (..)
  , HasBindHost (..)
  , HasBindPort (..)
  , HasExternalAddress (..)
  ) where

import Control.Lens (makeFieldsId)
import Network.Socket
import RIO

data KaisuiAddrInfo
  = KaisuiAddrInfo
  { bindHost :: HostName
  , bindPort :: ServiceName
  , externalAddress :: ServiceName -> (HostName, ServiceName)
  }
  deriving (Generic)

makeFieldsId ''KaisuiAddrInfo

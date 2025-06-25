module Network.Transport.Kaisui.Transport
  ( KaisuiTransport (..)
  , HasAddrInfo (..)
  , HasState (..)
  , HasParameters (..)
  , TransportState (..)
  ) where

import Control.Lens (makeFieldsId)
import Network.Socket
import Network.Transport.Kaisui.Parameters
import Network.Transport.Kaisui.TransportAddrInfo
import RIO

data KaisuiTransport
  = KaisuiTransport
  { addrInfo :: Maybe TransportAddrInfo
  , state :: MVar TransportState
  , parameters :: KaisuiParameters
  }
  deriving (Generic)

data TransportState
  = -- | dummy.
    TransportValid
  | TransportClosed

makeFieldsId ''KaisuiTransport

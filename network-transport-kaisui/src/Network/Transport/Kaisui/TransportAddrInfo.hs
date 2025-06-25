module Network.Transport.Kaisui.TransportAddrInfo
  ( TransportAddrInfo (..)
  , HasHost (..)
  , HasPort (..)
  , HasBindHost (..)
  , HasBindPort (..)
  ) where

import Control.Lens (makeFieldsId)
import Network.Socket
import Network.Transport.Kaisui.Parameters
import RIO

data TransportAddrInfo
  = TransportAddrInfo
  { host :: HostName
  , port :: ServiceName
  , bindHost :: HostName
  , bindPort :: ServiceName
  }

makeFieldsId ''TransportAddrInfo

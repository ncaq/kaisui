module Network.Transport.Kaisui.Type.EndPoint
  ( KaisuiEndPoint (..)
  , KaisuiEndPointMap
  , HasEndpointId (..)
  , HasSocket (..)
  , HasConnections (..)
  , HasReceiveQueue (..)
  , HasClosed (..)
  ) where

import Control.Lens (makeFieldsId)
import Network.Socket (Socket)
import qualified Network.Transport as NT
import Network.Transport.Kaisui.Type.Connection
import RIO
import qualified RIO.HashMap as HM

-- | Kaisui endpoint implementation
data KaisuiEndPoint = KaisuiEndPoint
  { endpointId :: NT.EndPointAddress
  , socket :: Socket
  , connections :: TVar KaisuiConnectionMap
  , receiveQueue :: TBQueue NT.Event
  , closed :: TVar Bool
  }

makeFieldsId ''KaisuiEndPoint

-- | Map of endpoint address to endpoint
type KaisuiEndPointMap = HM.HashMap NT.EndPointAddress KaisuiEndPoint

module Network.Transport.Kaisui.Type.Connection
  ( KaisuiConnection (..)
  , KaisuiConnectionMap
  , HasConnectionId (..)
  , HasReliability (..)
  , HasSocket (..)
  , HasRemoteAddr (..)
  , HasState (..)
  ) where

import Control.Lens (makeFieldsId)
import Network.Socket (SockAddr, Socket)
import qualified Network.Transport as NT
import Network.Transport.Kaisui.Type.ConnectionState
import RIO
import qualified RIO.HashMap as HM

-- | Kaisui connection implementation
data KaisuiConnection = KaisuiConnection
  { connectionId :: NT.ConnectionId
  , reliability :: NT.Reliability
  , socket :: Socket
  , remoteAddr :: SockAddr
  , state :: TVar ConnectionState
  }

-- | Map of connection ID to connection
type KaisuiConnectionMap = HM.HashMap NT.ConnectionId KaisuiConnection

makeFieldsId ''KaisuiConnection

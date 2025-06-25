module Network.Transport.Kaisui.EndPoint
  ( createNewEndPoint
  ) where

import Network.Transport
import Network.Transport.Kaisui.Parameters
import Network.Transport.Kaisui.Transport
import RIO

createNewEndPoint :: KaisuiTransport -> Maybe ThreadId -> IO (Either (TransportError NewEndPointErrorCode) EndPoint)
createNewEndPoint _ _ = undefined

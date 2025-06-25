module Network.Transport.Kaisui.CreateTransport
  ( createTransport
  ) where

import Network.Transport
import Network.Transport.Kaisui.Addr
import Network.Transport.Kaisui.EndPoint
import Network.Transport.Kaisui.Parameters
import Network.Transport.Kaisui.Transport
import RIO

-- | Create a Kaisui transport.
-- きれいではないが`network-transport`にシグネチャを合わせている。
createTransport :: KaisuiAddr -> KaisuiParameters -> IO (Either IOException Transport)
createTransport Unaddressable _ = do
  state' <- newMVar TransportValid
  let kaisuiTransport =
        KaisuiTransport
          { addrInfo = Nothing
          , state = state'
          , parameters = defaultKaisuiParameters
          }
      newEndPoint = createNewEndPoint kaisuiTransport Nothing
  return
    $ Right
      Transport
        { newEndPoint = newEndPoint
        , closeTransport = closeKaisuiTransport kaisuiTransport
        }
createTransport _ _ = undefined

closeKaisuiTransport :: KaisuiTransport -> IO ()
closeKaisuiTransport _ = undefined

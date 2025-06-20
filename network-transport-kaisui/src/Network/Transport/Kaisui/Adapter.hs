module Network.Transport.Kaisui.Adapter
  ( toTransport
  , toEndPoint
  , toConnection
  ) where

import Network.Transport (Connection (..), EndPoint (..), Transport (..))
import qualified Network.Transport as NT
import Network.Transport.Kaisui.Connection (closeKaisuiConnection, sendOnKaisuiConnection)
import qualified Network.Transport.Kaisui.EndPoint as EO
import Network.Transport.Kaisui.Type.Connection (KaisuiConnection)
import Network.Transport.Kaisui.Type.EndPoint (KaisuiEndPoint)
import qualified Network.Transport.Kaisui.Type.EndPoint as EP
import Network.Transport.Kaisui.Type.Transport (KaisuiTransport)
import RIO

-- | Convert internal transport to Network.Transport interface
toTransport
  :: KaisuiTransport
  -> (KaisuiTransport -> IO (Either (NT.TransportError NT.NewEndPointErrorCode) EndPoint))
  -> (KaisuiTransport -> IO ())
  -> Transport
toTransport kt newEndPointFunc closeFunc =
  Transport
    { newEndPoint = newEndPointFunc kt
    , closeTransport = closeFunc kt
    }

-- | Convert internal endpoint to Network.Transport interface
toEndPoint
  :: KaisuiEndPoint
  -> ( KaisuiEndPoint
       -> NT.EndPointAddress
       -> NT.Reliability
       -> NT.ConnectHints
       -> IO (Either (NT.TransportError NT.ConnectErrorCode) Connection)
     )
  -> EndPoint
toEndPoint kep connectFunc =
  EndPoint
    { receive = EO.receiveEvent kep
    , address = kep ^. EP.endpointId
    , connect = connectFunc kep
    , newMulticastGroup = unsupportedMulticast
    , resolveMulticastGroup = unsupportedResolve
    , closeEndPoint = EO.closeKaisuiEndPoint kep
    }

-- | Convert internal connection to Network.Transport interface
toConnection :: KaisuiConnection -> Connection
toConnection kc =
  Connection
    { send = try . sendOnKaisuiConnection kc . mconcat
    , close = closeKaisuiConnection kc
    }

-- | Return unsupported error for multicast operations
unsupportedMulticast :: IO (Either (NT.TransportError NT.NewMulticastGroupErrorCode) NT.MulticastGroup)
unsupportedMulticast = pure $ Left $ NT.TransportError NT.NewMulticastGroupUnsupported "Multicast not supported"

-- | Return unsupported error for multicast resolution
unsupportedResolve
  :: NT.MulticastAddress -> IO (Either (NT.TransportError NT.ResolveMulticastGroupErrorCode) NT.MulticastGroup)
unsupportedResolve _ = pure $ Left $ NT.TransportError NT.ResolveMulticastGroupUnsupported "Multicast not supported"

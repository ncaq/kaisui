module Network.Transport.Kaisui.Type.Transport
  ( KaisuiTransport (..)
  , HasEndPoints (..)
  , HasCounter (..)
  , HasHost (..)
  , HasPort (..)
  ) where

import Control.Lens (makeFieldsId)
import Network.Transport.Kaisui.Type.EndPoint
import RIO

-- | Kaisui transport implementation
data KaisuiTransport = KaisuiTransport
  { endPoints :: TVar KaisuiEndPointMap
  , counter :: TVar Word32
  , host :: Text
  , port :: Text
  }

makeFieldsId ''KaisuiTransport

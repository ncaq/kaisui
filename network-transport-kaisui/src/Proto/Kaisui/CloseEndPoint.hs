module Proto.Kaisui.CloseEndPoint
  ( CloseEndPoint (..)
  , HasEndpointId (..)
  ) where

import Control.Lens (makeFieldsId)
import qualified Proto3.Suite.Class as P
import qualified Proto3.Suite.DotProto as Pdot
import qualified Proto3.Suite.Types as P
import qualified Proto3.Wire as P
import RIO

-- | Close endpoint
newtype CloseEndPoint = CloseEndPoint
  { endpointId :: Text
  }
  deriving (Eq, Generic, Ord, Read, Show)

makeFieldsId ''CloseEndPoint

-- Protocol Buffer instances
instance P.Named CloseEndPoint where
  nameOf _ = "CloseEndPoint"

instance P.HasDefault CloseEndPoint

instance P.Message CloseEndPoint where
  encodeMessage _ msg =
    P.encodeMessageField
      (P.FieldNumber 1)
      (P.String (msg ^. endpointId))

  decodeMessage _ = do
    P.String endpointId' <- P.at P.decodeMessageField (P.FieldNumber 1)
    pure CloseEndPoint{endpointId = endpointId'}

  dotProto _ =
    [ Pdot.DotProtoField
        (P.FieldNumber 1)
        (Pdot.Prim Pdot.String)
        (Pdot.Single "endpointId")
        []
        ""
    ]

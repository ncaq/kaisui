module Proto.Kaisui.CloseEndPoint
  ( CloseEndPoint (..)
  , HasEndpointId (..)
  ) where

import Control.Lens (makeFieldsId)
import Data.Coerce (coerce)
import qualified Proto3.Suite.Class as P
import qualified Proto3.Suite.DotProto as Pdot
import qualified Proto3.Suite.Types as P
import qualified Proto3.Wire as P
import RIO
import qualified RIO.Text.Lazy as TL

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
  encodeMessage _ CloseEndPoint{..} =
    P.encodeMessageField
      (P.FieldNumber 1)
      (coerce @TL.Text @(P.String TL.Text) (TL.fromStrict endpointId))

  decodeMessage _ = do
    endpointId <-
      TL.toStrict
        <$> P.coerceOver @(P.String TL.Text) @TL.Text
          (P.at P.decodeMessageField (P.FieldNumber 1))
    pure CloseEndPoint{..}

  dotProto _ =
    [ Pdot.DotProtoField
        (P.FieldNumber 1)
        (Pdot.Prim Pdot.String)
        (Pdot.Single "endpointId")
        []
        ""
    ]

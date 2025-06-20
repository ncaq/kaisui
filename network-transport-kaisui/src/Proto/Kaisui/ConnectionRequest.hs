module Proto.Kaisui.ConnectionRequest
  ( ConnectionRequest (..)
  , HasFromEndpoint (..)
  , HasReliability (..)
  , HasConnectionId (..)
  ) where

import Control.Lens (makeFieldsId)
import Data.Coerce (coerce)
import qualified Proto3.Suite.Class as P
import qualified Proto3.Suite.DotProto as Pdot
import qualified Proto3.Suite.Types as P
import qualified Proto3.Wire as P
import RIO
import qualified RIO.Text.Lazy as TL

-- | Connection request
data ConnectionRequest = ConnectionRequest
  { fromEndpoint :: Text
  , reliability :: Word32
  , connectionId :: Text
  }
  deriving (Eq, Generic, Ord, Read, Show)

makeFieldsId ''ConnectionRequest

-- Protocol Buffer instances
instance P.Named ConnectionRequest where
  nameOf _ = "ConnectionRequest"

instance P.HasDefault ConnectionRequest

instance P.Message ConnectionRequest where
  encodeMessage _ ConnectionRequest{..} =
    P.encodeMessageField
      (P.FieldNumber 1)
      (coerce @TL.Text @(P.String TL.Text) (TL.fromStrict fromEndpoint))
      <> P.encodeMessageField (P.FieldNumber 2) reliability
      <> P.encodeMessageField
        (P.FieldNumber 3)
        (coerce @TL.Text @(P.String TL.Text) (TL.fromStrict connectionId))

  decodeMessage _ = do
    fromEndpoint <-
      TL.toStrict
        <$> P.coerceOver @(P.String TL.Text) @TL.Text
          (P.at P.decodeMessageField (P.FieldNumber 1))
    reliability <- P.at P.decodeMessageField (P.FieldNumber 2)
    connectionId <-
      TL.toStrict
        <$> P.coerceOver @(P.String TL.Text) @TL.Text
          (P.at P.decodeMessageField (P.FieldNumber 3))
    pure ConnectionRequest{..}

  dotProto _ =
    [ Pdot.DotProtoField
        (P.FieldNumber 1)
        (Pdot.Prim Pdot.String)
        (Pdot.Single "fromEndpoint")
        []
        ""
    , Pdot.DotProtoField
        (P.FieldNumber 2)
        (Pdot.Prim Pdot.UInt32)
        (Pdot.Single "reliability")
        []
        ""
    , Pdot.DotProtoField
        (P.FieldNumber 3)
        (Pdot.Prim Pdot.String)
        (Pdot.Single "connectionId")
        []
        ""
    ]

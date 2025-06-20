module Proto.Kaisui.DataMessage
  ( DataMessage (..)
  , HasConnectionId (..)
  , HasPayload (..)
  ) where

import Control.Lens (makeFieldsId)
import Data.Coerce (coerce)
import qualified Proto3.Suite.Class as P
import qualified Proto3.Suite.DotProto as Pdot
import Proto3.Suite.Types (Bytes (..))
import qualified Proto3.Suite.Types as P
import qualified Proto3.Wire as P
import RIO
import qualified RIO.ByteString.Lazy as LBS
import qualified RIO.Text.Lazy as TL

-- | Data message
data DataMessage = DataMessage
  { connectionId :: Text
  , payload :: ByteString
  }
  deriving (Eq, Generic, Ord, Read, Show)

makeFieldsId ''DataMessage

-- Protocol Buffer instances
instance P.Named DataMessage where
  nameOf _ = "DataMessage"

instance P.HasDefault DataMessage

instance P.Message DataMessage where
  encodeMessage _ DataMessage{..} =
    P.encodeMessageField
      (P.FieldNumber 1)
      (coerce @TL.Text @(P.String TL.Text) (TL.fromStrict connectionId))
      <> P.encodeMessageField (P.FieldNumber 2) (P.Bytes $ LBS.fromStrict payload)

  decodeMessage _ = do
    connectionId <-
      TL.toStrict
        <$> P.coerceOver @(P.String TL.Text) @TL.Text
          (P.at P.decodeMessageField (P.FieldNumber 1))
    Bytes payloadLbs <- P.at P.decodeMessageField (P.FieldNumber 2)
    let payload = LBS.toStrict payloadLbs
    pure DataMessage{..}

  dotProto _ =
    [ Pdot.DotProtoField
        (P.FieldNumber 1)
        (Pdot.Prim Pdot.String)
        (Pdot.Single "connectionId")
        []
        ""
    , Pdot.DotProtoField
        (P.FieldNumber 2)
        (Pdot.Prim Pdot.Bytes)
        (Pdot.Single "payload")
        []
        ""
    ]

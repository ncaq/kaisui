module Proto.Kaisui.DataMessage
  ( DataMessage (..)
  , HasConnectionId (..)
  , HasPayload (..)
  ) where

import Control.Lens (makeFieldsId)
import qualified Proto3.Suite.Class as P
import qualified Proto3.Suite.DotProto as Pdot
import Proto3.Suite.Types (Bytes (..))
import qualified Proto3.Suite.Types as P
import qualified Proto3.Wire as P
import RIO

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
  encodeMessage _ msg =
    P.encodeMessageField
      (P.FieldNumber 1)
      (P.String (msg ^. connectionId))
      <> P.encodeMessageField (P.FieldNumber 2) (Bytes (msg ^. payload))

  decodeMessage _ = do
    P.String connectionId' <- P.at P.decodeMessageField (P.FieldNumber 1)
    Bytes payload' <- P.at P.decodeMessageField (P.FieldNumber 2)
    pure
      DataMessage
        { connectionId = connectionId'
        , payload = payload'
        }

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

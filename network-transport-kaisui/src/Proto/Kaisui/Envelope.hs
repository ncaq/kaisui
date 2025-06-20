module Proto.Kaisui.Envelope
  ( Envelope (..)
  , EnvelopeMessage (..)
  , HasMessage (..)
  ) where

import Control.Lens (makeFieldsId)
import Proto.Kaisui.CloseConnection
import Proto.Kaisui.CloseEndPoint
import Proto.Kaisui.ConnectionRequest
import Proto.Kaisui.ConnectionResponse
import Proto.Kaisui.DataMessage
import qualified Proto3.Suite.Class as P
import qualified Proto3.Suite.DotProto as Pdot
import qualified Proto3.Suite.Types as P
import qualified Proto3.Wire as P
import RIO

-- | Envelope for all messages
newtype Envelope = Envelope
  { message :: Maybe EnvelopeMessage
  }
  deriving (Eq, Generic, Ord, Read, Show)

data EnvelopeMessage
  = ConnectionRequestMessage ConnectionRequest
  | ConnectionResponseMessage ConnectionResponse
  | DataMessageMessage DataMessage
  | CloseConnectionMessage CloseConnection
  | CloseEndPointMessage CloseEndPoint
  deriving (Eq, Generic, Ord, Read, Show)

makeFieldsId ''Envelope

-- Protocol Buffer instances
instance P.Named Envelope where
  nameOf _ = "Envelope"

instance P.HasDefault Envelope

instance P.Message Envelope where
  encodeMessage _ (Envelope mbMsg) =
    case mbMsg of
      Nothing -> mempty
      Just (ConnectionRequestMessage req) ->
        P.encodeMessageField (P.FieldNumber 1) (P.Nested (Just req))
      Just (ConnectionResponseMessage resp) ->
        P.encodeMessageField (P.FieldNumber 2) (P.Nested (Just resp))
      Just (DataMessageMessage msg) ->
        P.encodeMessageField (P.FieldNumber 3) (P.Nested (Just msg))
      Just (CloseConnectionMessage msg) ->
        P.encodeMessageField (P.FieldNumber 4) (P.Nested (Just msg))
      Just (CloseEndPointMessage msg) ->
        P.encodeMessageField (P.FieldNumber 5) (P.Nested (Just msg))

  decodeMessage _ = do
    msg <-
      P.oneof
        Nothing
        [ (1, fmap ConnectionRequestMessage . P.nested <$> P.decodeMessageField)
        , (2, fmap ConnectionResponseMessage . P.nested <$> P.decodeMessageField)
        , (3, fmap DataMessageMessage . P.nested <$> P.decodeMessageField)
        , (4, fmap CloseConnectionMessage . P.nested <$> P.decodeMessageField)
        , (5, fmap CloseEndPointMessage . P.nested <$> P.decodeMessageField)
        ]
    pure $ Envelope msg

  dotProto _ =
    [ Pdot.DotProtoField
        (P.FieldNumber 1)
        (Pdot.Prim (Pdot.Named (Pdot.Single "ConnectionRequest")))
        (Pdot.Single "connection_request")
        []
        ""
    , Pdot.DotProtoField
        (P.FieldNumber 2)
        (Pdot.Prim (Pdot.Named (Pdot.Single "ConnectionResponse")))
        (Pdot.Single "connection_response")
        []
        ""
    , Pdot.DotProtoField
        (P.FieldNumber 3)
        (Pdot.Prim (Pdot.Named (Pdot.Single "DataMessage")))
        (Pdot.Single "data_message")
        []
        ""
    , Pdot.DotProtoField
        (P.FieldNumber 4)
        (Pdot.Prim (Pdot.Named (Pdot.Single "CloseConnection")))
        (Pdot.Single "close_connection")
        []
        ""
    , Pdot.DotProtoField
        (P.FieldNumber 5)
        (Pdot.Prim (Pdot.Named (Pdot.Single "CloseEndPoint")))
        (Pdot.Single "close_endpoint")
        []
        ""
    ]

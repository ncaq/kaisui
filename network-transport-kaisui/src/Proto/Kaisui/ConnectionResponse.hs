module Proto.Kaisui.ConnectionResponse
  ( ConnectionResponse (..)
  , ConnectionResponseResult (..)
  , HasConnectionId (..)
  , HasResult (..)
  ) where

import Control.Lens (makeFieldsId)
import qualified Proto3.Suite.Class as P
import qualified Proto3.Suite.DotProto as Pdot
import qualified Proto3.Suite.Types as P
import qualified Proto3.Wire as P
import RIO

-- | Connection response
data ConnectionResponse = ConnectionResponse
  { connectionId :: Text
  , result :: Maybe ConnectionResponseResult
  }
  deriving (Eq, Generic, Ord, Read, Show)

data ConnectionResponseResult
  = ErrorResult Text
  | EndpointAddressResult Text
  deriving (Eq, Generic, Ord, Read, Show)

makeFieldsId ''ConnectionResponse

-- Protocol Buffer instances
instance P.Named ConnectionResponse where
  nameOf _ = "ConnectionResponse"

instance P.HasDefault ConnectionResponse

instance P.Message ConnectionResponse where
  encodeMessage _ msg =
    P.encodeMessageField
      (P.FieldNumber 1)
      (P.String (msg ^. connectionId))
      <> case msg ^. result of
        Just (ErrorResult err) ->
          P.encodeMessageField
            (P.FieldNumber 2)
            (P.String err)
        Just (EndpointAddressResult addr) ->
          P.encodeMessageField
            (P.FieldNumber 3)
            (P.String addr)
        Nothing -> mempty

  decodeMessage _ = do
    P.String connectionId' <- P.at P.decodeMessageField (P.FieldNumber 1)
    result' <-
      P.oneof
        Nothing
        [
          ( P.FieldNumber 2
          , do
              P.String err <- P.decodeMessageField
              pure $ Just $ ErrorResult err
          )
        ,
          ( P.FieldNumber 3
          , do
              P.String addr <- P.decodeMessageField
              pure $ Just $ EndpointAddressResult addr
          )
        ]
    pure ConnectionResponse{connectionId = connectionId', result = result'}

  dotProto _ =
    [ Pdot.DotProtoField
        (P.FieldNumber 1)
        (Pdot.Prim Pdot.String)
        (Pdot.Single "connectionId")
        []
        ""
    , Pdot.DotProtoField
        (P.FieldNumber 2)
        (Pdot.Prim Pdot.String)
        (Pdot.Single "error")
        []
        ""
    , Pdot.DotProtoField
        (P.FieldNumber 3)
        (Pdot.Prim Pdot.String)
        (Pdot.Single "endpointAddress")
        []
        ""
    ]

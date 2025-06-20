module Proto.Kaisui.ConnectionResponse
  ( ConnectionResponse (..)
  , ConnectionResponseResult (..)
  , HasConnectionId (..)
  , HasResult (..)
  ) where

import Control.Lens (makeFieldsId)
import Data.Coerce (coerce)
import qualified Proto3.Suite.Class as P
import qualified Proto3.Suite.DotProto as Pdot
import qualified Proto3.Suite.Types as P
import qualified Proto3.Wire as P
import RIO
import qualified RIO.Text.Lazy as TL

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
  encodeMessage _ ConnectionResponse{..} =
    P.encodeMessageField
      (P.FieldNumber 1)
      (coerce @TL.Text @(P.String TL.Text) (TL.fromStrict connectionId))
      <> case result of
        Just (ErrorResult err) ->
          P.encodeMessageField
            (P.FieldNumber 2)
            (coerce @TL.Text @(P.String TL.Text) (TL.fromStrict err))
        Just (EndpointAddressResult addr) ->
          P.encodeMessageField
            (P.FieldNumber 3)
            (coerce @TL.Text @(P.String TL.Text) (TL.fromStrict addr))
        Nothing -> mempty

  decodeMessage _ = do
    connectionId <-
      TL.toStrict
        <$> P.coerceOver @(P.String TL.Text) @TL.Text
          (P.at P.decodeMessageField (P.FieldNumber 1))
    result <-
      P.oneof
        Nothing
        [
          ( P.FieldNumber 2
          , Just
              . ErrorResult
              . TL.toStrict
              . coerce @(P.String TL.Text) @TL.Text
              <$> P.decodeMessageField
          )
        ,
          ( P.FieldNumber 3
          , Just
              . EndpointAddressResult
              . TL.toStrict
              . coerce @(P.String TL.Text) @TL.Text
              <$> P.decodeMessageField
          )
        ]
    pure ConnectionResponse{..}

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

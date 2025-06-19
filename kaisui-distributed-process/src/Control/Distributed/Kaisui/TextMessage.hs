{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Control.Distributed.Kaisui.TextMessage
  ( TextMessage (..)
  , encodeProtobuf
  , decodeProtobuf
  , module Export
  ) where

import Control.Applicative ((<$>), (<*>), (<|>))
import qualified Control.Applicative as Hs
import Control.Distributed.Kaisui.HasClass as Export
import Control.Lens (makeFieldsId)
import Data.Binary (Binary)
import qualified Data.Coerce as Hs
import qualified Data.Proxy as Proxy
import qualified Data.String as Hs (fromString)
import qualified Google.Protobuf.Wrappers.Polymorphic as HsProtobuf (Wrapped (..))
import qualified Proto3.Suite.Class as HsProtobuf
import qualified Proto3.Suite.DotProto as HsProtobufAST
import Proto3.Suite.JSONPB ((.:), (.=))
import qualified Proto3.Suite.JSONPB as HsJSONPB
import qualified Proto3.Suite.Types as HsProtobuf
import qualified Proto3.Wire as HsProtobuf
import qualified Proto3.Wire.Decode as HsProtobuf (Parser, RawField)
import RIO
import qualified RIO.ByteString as BS
import qualified RIO.ByteString.Lazy as LBS
import qualified RIO.Text as T
import qualified RIO.Text.Lazy as TL

-- | Message type for Text communication with Protocol Buffer support
newtype TextMessage
  = TextMessage {body :: Text}
  deriving stock (Eq, Generic, Ord, Read, Show, Typeable)
  deriving newtype (NFData)

-- For backward compatibility with Binary
instance Binary TextMessage

-- Protocol Buffer instances
instance HsProtobuf.Named TextMessage where
  nameOf _ = Hs.fromString "TextMessage"

instance HsProtobuf.HasDefault TextMessage

instance HsProtobuf.Message TextMessage where
  encodeMessage _ TextMessage{body} =
    HsProtobuf.encodeMessageField
      (HsProtobuf.FieldNumber 1)
      (Hs.coerce @TL.Text @(HsProtobuf.String TL.Text) (TL.fromStrict body))
  decodeMessage _ =
    TextMessage . TL.toStrict
      <$> HsProtobuf.coerceOver @(HsProtobuf.String TL.Text) @TL.Text
        ( HsProtobuf.at
            HsProtobuf.decodeMessageField
            (HsProtobuf.FieldNumber 1)
        )
  dotProto _ =
    [ HsProtobufAST.DotProtoField
        (HsProtobuf.FieldNumber 1)
        (HsProtobufAST.Prim HsProtobufAST.String)
        (HsProtobufAST.Single "body")
        []
        ""
    ]

-- Helper functions for Protocol Buffer encoding/decoding
encodeProtobuf :: TextMessage -> ByteString
encodeProtobuf msg = LBS.toStrict $ HsProtobuf.toLazyByteString msg

decodeProtobuf :: ByteString -> Either String TextMessage
decodeProtobuf bs = case HsProtobuf.fromByteString bs of
  Left err -> Left (show err)
  Right msg -> Right msg

makeFieldsId ''TextMessage

-- | Error types and throwing functions for the Kaisui transport implementation.
--
-- This module defines a hierarchical error type system for all errors that can
-- occur in the Kaisui transport layer. Errors are organized into three main categories:
--
-- * __Protocol errors__: Violations of the Kaisui protocol specification
-- * __Serialization errors__: Failures in message encoding/decoding
-- * __Network errors__: Low-level network and socket failures
--
-- = Usage
--
-- Instead of throwing generic exceptions, use the specific error throwing functions
-- provided in this module:
--
-- @
-- -- Throw a protocol error when receiving an invalid message
-- throwInvalidMessageType 0xFF
--
-- -- Throw a serialization error when decoding fails
-- throwDecodingFailed "Invalid protobuf message" rawBytes
--
-- -- Throw a network error on connection failure
-- throwConnectionRefused "localhost:8080"
-- @
module Network.Transport.Kaisui.Error
  ( -- * Error types
    KaisuiError
  , ProtocolError
  , SerializationError
  , NetworkError

    -- * Throwing errors
  , throwProtocolError
  , throwInvalidMessageType
  , throwUnexpectedMessage
  , throwProtocolVersionMismatch
  , throwDecodingFailed
  , throwEncodingFailed
  , throwNetworkError
  , throwSocketError
  , throwAddressError
  , throwConnectionRefused
  , throwConnectionTimeout
  ) where

import Control.Lens (makeClassyPrisms)
import Data.Convertible
import RIO

-- | Root error type for Kaisui transport implementation.
--
-- This type encompasses all possible errors that can occur in the Kaisui transport layer,
-- organized into three main categories: protocol errors, serialization errors, and network errors.
data KaisuiError
  = -- | Protocol-level errors such as invalid messages or version mismatches
    KaisuiProtocolError ProtocolError
  | -- | Errors that occur during message encoding/decoding
    KaisuiSerializationError SerializationError
  | -- | Network-level errors such as socket failures or connection issues
    KaisuiNetworkError NetworkError
  deriving (Eq, Generic, Ord, Read, Show)

-- | Protocol-level errors that occur during communication.
--
-- These errors indicate violations of the Kaisui protocol specification
-- or unexpected message sequences.
data ProtocolError
  = -- | The invalid message type byte received
    InvalidMessageType
      Word8
  | -- | Description of the unexpected message
    UnexpectedMessage
      Text
  | ProtocolVersionMismatch
      Word32
      -- ^ Expected protocol version
      Word32
      -- ^ Actual protocol version received
  deriving (Eq, Generic, Ord, Read, Show)

-- | Serialization errors that occur during message encoding/decoding.
--
-- These errors indicate that a message could not be properly serialized
-- or deserialized according to the protocol buffer specification.
data SerializationError
  = DecodingFailed
      Text
      -- ^ Error message describing the decoding failure
      ByteString
      -- ^ The raw input that failed to decode
  | -- | Error message describing the encoding failure
    EncodingFailed
      Text
  deriving (Eq, Generic, Ord, Read, Show)

-- | Network-level errors related to socket operations and connections.
--
-- These errors represent failures in the underlying network transport layer,
-- such as socket errors, address resolution failures, or connection issues.
data NetworkError
  = -- | Error message from the socket operation
    SocketError
      Text
  | AddressError
      Text
      -- ^ The address that caused the error
      Text
      -- ^ Reason for the address error
  | -- | The endpoint that refused the connection
    ConnectionRefused
      Text
  | ConnectionTimeout
      Text
      -- ^ The endpoint that timed out
      Int
      -- ^ Timeout duration in milliseconds
  deriving (Eq, Generic, Ord, Read, Show)

instance NFData KaisuiError
instance NFData ProtocolError
instance NFData SerializationError
instance NFData NetworkError

instance Exception KaisuiError
instance Exception ProtocolError
instance Exception SerializationError
instance Exception NetworkError

makeClassyPrisms ''KaisuiError

-- | Throw a protocol error
throwProtocolError :: (MonadThrow m) => ProtocolError -> m a
throwProtocolError = throwM . KaisuiProtocolError

-- | Throw an invalid message type error
throwInvalidMessageType :: (MonadThrow m) => Word8 -> m a
throwInvalidMessageType msgType = throwProtocolError $ InvalidMessageType msgType

-- | Throw an unexpected message error
throwUnexpectedMessage :: (MonadThrow m) => Text -> m a
throwUnexpectedMessage msg = throwProtocolError $ UnexpectedMessage msg

-- | Throw a protocol version mismatch error
throwProtocolVersionMismatch :: (MonadThrow m) => Word32 -> Word32 -> m a
throwProtocolVersionMismatch expectedVer actualVer = throwProtocolError $ ProtocolVersionMismatch expectedVer actualVer

-- | Throw a serialization error
throwSerializationError :: (MonadThrow m) => SerializationError -> m a
throwSerializationError = throwM . KaisuiSerializationError

-- | Throw a decoding failed error
throwDecodingFailed :: (MonadThrow m) => Text -> ByteString -> m a
throwDecodingFailed msg input = throwSerializationError $ DecodingFailed msg input

-- | Throw an encoding failed error
throwEncodingFailed :: (MonadThrow m) => Text -> m a
throwEncodingFailed msg = throwSerializationError $ EncodingFailed msg

-- | Throw a network error
throwNetworkError :: (MonadThrow m) => NetworkError -> m a
throwNetworkError = throwM . KaisuiNetworkError

-- | Throw a socket error
throwSocketError :: (MonadThrow m) => SomeException -> m a
throwSocketError e = throwNetworkError $ SocketError (convert $ displayException e)

-- | Throw an address error
throwAddressError :: (MonadThrow m) => Text -> Text -> m a
throwAddressError addr reason = throwNetworkError $ AddressError addr reason

-- | Throw a connection refused error
throwConnectionRefused :: (MonadThrow m) => Text -> m a
throwConnectionRefused endpoint = throwNetworkError $ ConnectionRefused endpoint

-- | Throw a connection timeout error
throwConnectionTimeout :: (MonadThrow m) => Text -> Int -> m a
throwConnectionTimeout endpoint timeoutMs = throwNetworkError $ ConnectionTimeout endpoint timeoutMs

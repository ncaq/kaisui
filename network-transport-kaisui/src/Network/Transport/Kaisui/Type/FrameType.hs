module Network.Transport.Kaisui.Type.FrameType
  ( FrameType (..)
  ) where

import RIO

-- | Frame types for Kaisui protocol
data FrameType
  = ProtoBufFrame -- Protocol Buffer message
  | HeartbeatFrame -- Keep-alive
  | ErrorFrame -- Error message
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

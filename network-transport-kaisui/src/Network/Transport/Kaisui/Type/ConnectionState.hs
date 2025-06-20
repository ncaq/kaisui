module Network.Transport.Kaisui.Type.ConnectionState
  ( ConnectionState (..)
  ) where

import RIO

-- | Connection state
data ConnectionState
  = ConnectionEstablished
  | ConnectionClosed
  | ConnectionPending
  deriving (Eq, Generic, Ord, Read, Show)

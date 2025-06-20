module Network.Transport.Kaisui.Type.TransportState
  ( TransportState (..)
  ) where

import RIO

-- | Transport state
data TransportState
  = TransportOpen
  | TransportClosed
  deriving (Eq, Generic, Ord, Read, Show)

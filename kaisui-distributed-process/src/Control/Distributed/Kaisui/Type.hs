module Control.Distributed.Kaisui.Type
  ( TextMessage (..)
  ) where

import Data.Binary (Binary)
import RIO

-- | Message type for Text communication
newtype TextMessage = TextMessage Text
  deriving (Eq, Ord, Read, Show, Generic, Typeable)

instance Binary TextMessage

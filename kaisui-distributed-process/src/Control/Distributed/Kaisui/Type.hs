module Control.Distributed.Kaisui.Type
  ( TextMessage (..)
  ) where

import Data.Binary (Binary)
import RIO

-- | Message type for Text communication
newtype TextMessage = TextMessage Text
  deriving (Eq, Generic, Ord, Read, Show, Typeable)

instance Binary TextMessage

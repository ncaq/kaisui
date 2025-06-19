module Control.Distributed.Kaisui.TextMessage
  ( TextMessage (..)
  , module Export
  ) where

import Control.Distributed.Kaisui.HasClass as Export
import Control.Lens (makeFieldsId)
import Data.Binary (Binary)
import RIO

-- | Message type for Text communication
newtype TextMessage
  = TextMessage
  { body :: Text
  }
  deriving (Eq, Generic, Ord, Read, Show, Typeable)

instance Binary TextMessage

makeFieldsId ''TextMessage

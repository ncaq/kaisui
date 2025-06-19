module Control.Distributed.Kaisui.TextMessagePB
  ( TextMessagePB (..)
  , module Export
  ) where

import Control.Distributed.Kaisui.HasClass as Export
import Control.Lens (makeFieldsId)
import Proto3.Suite
import RIO

-- Protocol Buffer message definitions
newtype TextMessagePB = TextMessagePB
  { body :: Text
  }
  deriving (Eq, Generic, Ord, Read, Show)

instance Message TextMessagePB
instance Named TextMessagePB

makeFieldsId ''TextMessagePB

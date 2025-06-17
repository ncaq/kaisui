module Control.Distributed.Kaisui.Types
  ( TextMessage (..)
  ) where

import Data.Binary (Binary)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

-- | Message type for Text communication
newtype TextMessage = TextMessage Text
  deriving (Eq, Ord, Read, Show, Generic, Typeable)

instance Binary TextMessage

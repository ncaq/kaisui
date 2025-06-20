module Network.Transport.Kaisui.Type.FrameHeader
  ( FrameHeader (..)

    -- * Lens classes
  , HasFrameType (..)
  , HasFrameLength (..)
  ) where

import Control.Lens (makeFieldsId)
import Network.Transport.Kaisui.Type.FrameType
import RIO

-- | Frame header
data FrameHeader = FrameHeader
  { frameType :: FrameType
  , frameLength :: Word32
  }
  deriving (Eq, Generic, Ord, Read, Show)

makeFieldsId ''FrameHeader

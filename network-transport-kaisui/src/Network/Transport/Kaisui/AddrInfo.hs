module Network.Transport.Kaisui.AddrInfo
  ( KaisuiAddrInfo (..)
  ) where

import Control.Lens (makeFieldsId)
import RIO

data KaisuiAddrInfo

makeFieldsId ''KaisuiAddrInfo

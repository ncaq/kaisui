module Network.Transport.Kaisui.Parameters
  ( KaisuiParameters (..)
  , defaultKaisuiParameters
  ) where

import Control.Lens (makeFieldsId)
import RIO

-- | Parameters for configuring Kaisui transport.
data KaisuiParameters

makeFieldsId ''KaisuiParameters

-- | Default parameters for Kaisui transport.
defaultKaisuiParameters :: KaisuiParameters
defaultKaisuiParameters = undefined

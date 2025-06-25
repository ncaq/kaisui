module Network.Transport.Kaisui.Parameters
  ( KaisuiParameters (..)
  , defaultKaisuiParameters
  ) where

-- | Parameters for configuring Kaisui transport.
-- 現状は設定する項目が無い。
data KaisuiParameters
  = KaisuiParameters

-- | Default parameters for Kaisui transport.
defaultKaisuiParameters :: KaisuiParameters
defaultKaisuiParameters = KaisuiParameters

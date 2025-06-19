module Control.Distributed.Kaisui.HasClass
  ( HasBody (..)
  ) where

import RIO

class HasBody a b where
  body :: Lens' a b

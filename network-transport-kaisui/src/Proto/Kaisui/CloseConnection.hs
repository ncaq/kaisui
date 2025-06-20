module Proto.Kaisui.CloseConnection
  ( CloseConnection (..)
  , HasConnectionId (..)
  ) where

import Control.Lens (makeFieldsId)
import qualified Proto3.Suite.Class as P
import qualified Proto3.Suite.DotProto as Pdot
import qualified Proto3.Suite.Types as P
import qualified Proto3.Wire as P
import RIO

-- | Close connection
newtype CloseConnection = CloseConnection
  { connectionId :: Text
  }
  deriving (Eq, Generic, Ord, Read, Show)

makeFieldsId ''CloseConnection

-- Protocol Buffer instances
instance P.Named CloseConnection where
  nameOf _ = "CloseConnection"

instance P.HasDefault CloseConnection

instance P.Message CloseConnection where
  encodeMessage _ msg =
    P.encodeMessageField
      (P.FieldNumber 1)
      (P.String (msg ^. connectionId))

  decodeMessage _ = do
    P.String connectionId' <- P.at P.decodeMessageField (P.FieldNumber 1)
    pure CloseConnection{connectionId = connectionId'}

  dotProto _ =
    [ Pdot.DotProtoField
        (P.FieldNumber 1)
        (Pdot.Prim Pdot.String)
        (Pdot.Single "connectionId")
        []
        ""
    ]

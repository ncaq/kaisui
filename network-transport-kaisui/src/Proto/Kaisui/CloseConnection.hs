module Proto.Kaisui.CloseConnection
  ( CloseConnection (..)
  , HasConnectionId (..)
  ) where

import Control.Lens (makeFieldsId)
import Data.Coerce (coerce)
import qualified Proto3.Suite.Class as P
import qualified Proto3.Suite.DotProto as Pdot
import qualified Proto3.Suite.Types as P
import qualified Proto3.Wire as P
import RIO
import qualified RIO.Text.Lazy as TL

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
  encodeMessage _ CloseConnection{..} =
    P.encodeMessageField
      (P.FieldNumber 1)
      (coerce @TL.Text @(P.String TL.Text) (TL.fromStrict connectionId))

  decodeMessage _ = do
    connectionId <-
      TL.toStrict
        <$> P.coerceOver @(P.String TL.Text) @TL.Text
          (P.at P.decodeMessageField (P.FieldNumber 1))
    pure CloseConnection{..}

  dotProto _ =
    [ Pdot.DotProtoField
        (P.FieldNumber 1)
        (Pdot.Prim Pdot.String)
        (Pdot.Single "connectionId")
        []
        ""
    ]

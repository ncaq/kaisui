module Network.Transport.Kaisui.HasLens
  ( HasId (..)
  , HasReliability (..)
  , HasSocket (..)
  , HasRemoteAddr (..)
  , HasState (..)
  , HasConnections (..)
  , HasReceiveQueue (..)
  , HasEndPoints (..)
  , HasCounter (..)
  , HasHost (..)
  , HasPort (..)
  , HasFromEndpoint (..)
  , HasConnectionId (..)
  , HasResult (..)
  , HasPayload (..)
  , HasEndpointId (..)
  , HasMessage (..)
  , HasFrameType (..)
  , HasFrameLength (..)
  ) where

import RIO

class HasId s a | s -> a where
  id :: Lens' s a

class HasReliability s a | s -> a where
  reliability :: Lens' s a

class HasSocket s a | s -> a where
  socket :: Lens' s a

class HasRemoteAddr s a | s -> a where
  remoteAddr :: Lens' s a

class HasState s a | s -> a where
  state :: Lens' s a

class HasConnections s a | s -> a where
  connections :: Lens' s a

class HasReceiveQueue s a | s -> a where
  receiveQueue :: Lens' s a

class HasEndPoints s a | s -> a where
  endPoints :: Lens' s a

class HasCounter s a | s -> a where
  counter :: Lens' s a

class HasHost s a | s -> a where
  host :: Lens' s a

class HasPort s a | s -> a where
  port :: Lens' s a

class HasFromEndpoint s a | s -> a where
  fromEndpoint :: Lens' s a

class HasConnectionId s a | s -> a where
  connectionId :: Lens' s a

class HasResult s a | s -> a where
  result :: Lens' s a

class HasPayload s a | s -> a where
  payload :: Lens' s a

class HasEndpointId s a | s -> a where
  endpointId :: Lens' s a

class HasMessage s a | s -> a where
  message :: Lens' s a

class HasFrameType s a | s -> a where
  frameType :: Lens' s a

class HasFrameLength s a | s -> a where
  frameLength :: Lens' s a

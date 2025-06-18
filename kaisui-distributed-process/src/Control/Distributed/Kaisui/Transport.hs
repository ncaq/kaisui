module Control.Distributed.Kaisui.Transport
  ( createNode
  , closeNodeSafely
  ) where

import Control.Distributed.Process.Node
import Network.Transport.TCP

-- | Create a new local node with TCP transport
createNode :: String -> String -> IO (Either String LocalNode)
createNode host port = do
  transportResult <- createTransport (defaultTCPAddr host port) defaultTCPParameters
  case transportResult of
    Left err -> return $ Left $ "Failed to create transport: " ++ show err
    Right transport -> do
      node <- newLocalNode transport initRemoteTable
      return $ Right node

-- | Safely close a node and its transport
closeNodeSafely :: LocalNode -> IO ()
closeNodeSafely node = do
  closeLocalNode node

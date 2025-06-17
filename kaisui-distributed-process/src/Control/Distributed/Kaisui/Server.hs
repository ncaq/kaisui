module Control.Distributed.Kaisui.Server
  ( runServer
  ) where

import Control.Distributed.Kaisui.Transport
import Control.Distributed.Kaisui.Types
import Control.Distributed.Process
import Control.Distributed.Process.Node
import qualified Data.Text as T

-- | Run a distributed process server
runServer :: String -> String -> IO ()
runServer host port = do
  putStrLn $ "Starting distributed server on " ++ host ++ ":" ++ port
  nodeResult <- createNode host port
  case nodeResult of
    Left err -> putStrLn err
    Right node -> do
      runProcess node runServerProcess
      closeNodeSafely node

-- | Server process implementation
runServerProcess :: Process ()
runServerProcess = do
  self <- getSelfPid
  register "text_server" self
  say $ "Server process started with PID: " ++ show self
  say "Server registered as 'text_server'"
  say "Server waiting for text messages..."
  serverLoop

-- | Main server loop that handles incoming messages
serverLoop :: Process ()
serverLoop = do
  receiveWait
    [ match $ \(sender, TextMessage msg) -> do
        say $ "Server received from " ++ show sender ++ ": " ++ T.unpack msg
        let response = TextMessage ("Echo: " <> msg)
        send sender response
        say $ "Server sent response: " ++ T.unpack ("Echo: " <> msg)
        serverLoop
    ]

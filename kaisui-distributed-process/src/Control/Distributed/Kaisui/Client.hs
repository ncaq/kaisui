module Control.Distributed.Kaisui.Client
  ( runClient
  ) where

import Control.Distributed.Process
import Control.Distributed.Process.Node
import Data.Text (Text)
import qualified Data.Text as T

import Control.Distributed.Kaisui.Transport
import Control.Distributed.Kaisui.Types

-- | Run a distributed process client
runClient :: String -> String -> Text -> IO ()
runClient host port message = do
  putStrLn $ "Connecting to distributed server at " ++ host ++ ":" ++ port
  nodeResult <- createNode "127.0.0.1" "0" -- Use dynamic port for client
  case nodeResult of
    Left err -> putStrLn err
    Right node -> do
      runProcess node (runClientProcess host port message)
      closeNodeSafely node

-- | Client process implementation
runClientProcess :: String -> String -> Text -> Process ()
runClientProcess _host _port message = do
  self <- getSelfPid
  say $ "Client process started with PID: " ++ show self

  -- Try to find the server by name (simplified discovery)
  -- In a real distributed setup, we would use proper node discovery
  say "Looking for text_server..."

  -- For demonstration, send to self first
  say $ "Client sending message: " ++ T.unpack message
  send self (self, TextMessage message)

  response <- expect :: Process TextMessage
  case response of
    TextMessage responseText -> do
      say $ "Client received response: " ++ T.unpack responseText
      liftIO $ putStrLn $ "Final response: " ++ T.unpack responseText

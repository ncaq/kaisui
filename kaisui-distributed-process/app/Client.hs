module Main
  ( main
  ) where

import Control.Distributed.Kaisui.Client
import Data.Convertible
import RIO
import System.Environment (getArgs)

main :: IO ()
main = runSimpleApp $ do
  args <- liftIO getArgs
  let (host, port, message) = case args of
        [h, p, m] -> (h, p, m)
        [h, p] -> (h, p, "Hello from Haskell client!")
        [m] -> ("127.0.0.1", "8080", m)
        _ -> ("127.0.0.1", "8080", "Hello from Haskell client!")

  logInfo
    $ "Haskell distributed-process client sending to "
    <> display (convert host :: Text)
    <> ":"
    <> display (convert port :: Text)
  logInfo $ "Message: " <> display (convert message :: Text)

  runClient (convert host) (convert port) (convert message)

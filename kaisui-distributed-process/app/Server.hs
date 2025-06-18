module Main
  ( main
  ) where

import Control.Distributed.Kaisui.Server
import Data.Convertible
import RIO
import System.Environment (getArgs)

main :: IO ()
main = runSimpleApp $ do
  args <- liftIO getArgs
  let (host, port) = case args of
        [h, p] -> (h, p)
        _ -> ("127.0.0.1", "8080")

  logInfo
    $ "Starting Haskell distributed-process server on "
    <> display (convert host :: Text)
    <> ":"
    <> display (convert port :: Text)
  logInfo "This server will receive text messages and echo them back."
  logInfo "Press Ctrl+C to stop."

  runServer (convert host) (convert port)

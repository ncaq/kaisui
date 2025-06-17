module Main
  ( main
  ) where

import Control.Distributed.Kaisui.Server
import Data.Convertible
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let (host, port) = case args of
        [h, p] -> (h, p)
        _ -> ("127.0.0.1", "8080")

  putStrLn $ "Starting Haskell distributed-process server on " ++ host ++ ":" ++ port
  putStrLn "This server will receive text messages and echo them back."
  putStrLn "Press Ctrl+C to stop."

  runServer (convert host) (convert port)

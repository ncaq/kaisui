module Main
  ( main
  ) where

import Control.Distributed.Kaisui.Client
import Data.Convertible
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let (host, port, message) = case args of
        [h, p, m] -> (h, p, m)
        [h, p] -> (h, p, "Hello from Haskell client!")
        [m] -> ("127.0.0.1", "8080", m)
        _ -> ("127.0.0.1", "8080", "Hello from Haskell client!")

  putStrLn $ "Haskell distributed-process client sending to " ++ host ++ ":" ++ port
  putStrLn $ "Message: " ++ message

  runClient (convert host) (convert port) (convert message)

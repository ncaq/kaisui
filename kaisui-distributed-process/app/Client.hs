module Main
  ( main
  ) where

import qualified Control.Distributed.Kaisui.Client as Kaisui
import qualified Data.Text as T
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let (host, port, message) = case args of
        [h, p, m] -> (h, p, T.pack m)
        [h, p] -> (h, p, "Hello from Haskell client!")
        [m] -> ("127.0.0.1", "8080", T.pack m)
        _ -> ("127.0.0.1", "8080", "Hello from Haskell client!")

  putStrLn $ "Haskell distributed-process client sending to " ++ host ++ ":" ++ port
  putStrLn $ "Message: " ++ T.unpack message

  Kaisui.runClient host port message

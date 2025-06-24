module Main where

import Data.Binary.Put
import Network.Socket
import Network.Socket.ByteString
import Proto.RactorCluster.Auth
import Proto.RactorCluster.Meta
import qualified Proto3.Suite.Class as P
import RIO
import qualified RIO.ByteString as BS
import qualified RIO.ByteString.Lazy as BL

main :: IO ()
main = runSimpleApp $ do
  logInfo "Starting simple connection test to ractor server"

  -- Connect to kaisui-ractor server
  let host = "127.0.0.1"
      port = "8080"

  addrInfo <- liftIO $ getAddrInfo Nothing (Just host) (Just port)
  case addrInfo of
    [] -> throwString "No address found"
    (addr : _) -> do
      sock <- liftIO $ socket (addrFamily addr) Stream defaultProtocol
      liftIO $ connect sock (addrAddress addr)
      logInfo $ "Connected to " <> displayShow host <> ":" <> displayShow port

      -- Send initial authentication message (NameMessage)
      let nameMsg =
            NameMessage
              { nameMessageName = "haskell-test@localhost"
              , nameMessageFlags = Just $ NodeFlags{nodeFlagsVersion = 1}
              , nameMessageConnectionString = "127.0.0.1:9999"
              }
          authMsg =
            AuthenticationMessage
              { authenticationMessageMsg = Just $ AuthenticationMessageMsgName nameMsg
              }
          networkMsg =
            NetworkMessage
              { networkMessageMessage = Just $ NetworkMessageMessageAuth authMsg
              }
          -- Encode the protobuf message
          protoBytes = BL.toStrict $ P.toLazyByteString networkMsg
          -- Calculate length and create length prefix (8 bytes, big-endian)
          len = fromIntegral (BS.length protoBytes) :: Word64
          lenBytes = BL.toStrict $ runPut $ putWord64be len
          -- Combine length prefix and message
          fullMessage = lenBytes <> protoBytes

      liftIO $ sendAll sock fullMessage
      logInfo $ "Sent NameMessage with length prefix: " <> display len <> " bytes"

      -- Try to receive response (may timeout)
      response <- liftIO $ recv sock 1024
      if BS.null response
        then logInfo "No response received"
        else logInfo $ "Received " <> display (BS.length response) <> " bytes"

      liftIO $ close sock
      logInfo "Test completed"

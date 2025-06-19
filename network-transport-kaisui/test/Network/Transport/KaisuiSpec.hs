module Network.Transport.KaisuiSpec (spec) where

import Network.Transport.Kaisui
import RIO
import Test.Syd

spec :: Spec
spec = describe "Network.Transport.Kaisui" $ do
  it "should create and close transport" $ do
    result <- createTransport "127.0.0.1" "8080"
    case result of
      Left err -> expectationFailure $ "Failed to create transport: " <> show err
      Right transport -> do
        closeTransport transport
        pure ()

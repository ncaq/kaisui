module Network.Transport.KaisuiSpec (spec) where

import Network.Transport (closeTransport)
import Network.Transport.Kaisui (createTransport)
import RIO
import Test.Syd

spec :: Spec
spec = describe "Network.Transport.Kaisui" $ do
  it "should create and close transport" $ do
    transport <- createTransport "127.0.0.1" "8080"
    closeTransport transport

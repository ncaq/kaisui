module Network.Transport.Kaisui.ConnectionSpec
  ( spec
  ) where

import qualified Network.Socket as NS
import Network.Transport (ConnectionId)
import qualified Network.Transport as NT
import Network.Transport.Kaisui.Connection
import Network.Transport.Kaisui.TestHelper (TestReliability (..))
import Network.Transport.Kaisui.Type.Connection hiding (socket)
import qualified Network.Transport.Kaisui.Type.Connection as KConn
import Network.Transport.Kaisui.Type.ConnectionState
import RIO
import Test.QuickCheck
import Test.Syd

spec :: Spec
spec = describe "Kaisui Connection" $ do
  describe "newKaisuiConnection" $ do
    it "should create a connection with given parameters" $ do
      -- Create a dummy socket and address
      sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
      let addr = NS.SockAddrInet 8080 (NS.tupleToHostAddress (127, 0, 0, 1))
          cid = 1001 :: ConnectionId
          rel = NT.ReliableOrdered

      conn <- atomically $ newKaisuiConnection cid rel sock addr

      -- Verify fields
      conn ^. connectionId `shouldBe` cid
      conn ^. reliability `shouldBe` rel
      conn ^. KConn.socket `shouldBe` sock
      conn ^. remoteAddr `shouldBe` addr

      -- Verify initial state
      currentState <- readTVarIO (conn ^. state)
      currentState `shouldBe` ConnectionEstablished

      -- Cleanup
      NS.close sock

    it "should work with different reliabilities"
      $ property
      $ \(TestReliability rel) -> do
        sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
        let addr = NS.SockAddrInet 8080 (NS.tupleToHostAddress (127, 0, 0, 1))
            cid = 1002 :: ConnectionId

        conn <- atomically $ newKaisuiConnection cid rel sock addr
        conn ^. reliability `shouldBe` rel

        -- Cleanup
        NS.close sock

  describe "closeKaisuiConnection" $ do
    it "should close an open connection" $ do
      sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
      let addr = NS.SockAddrInet 8080 (NS.tupleToHostAddress (127, 0, 0, 1))
          cid = 1003 :: ConnectionId
          rel = NT.ReliableOrdered

      conn <- atomically $ newKaisuiConnection cid rel sock addr

      -- Close the connection
      closeKaisuiConnection conn

      -- Verify state changed
      currentState <- readTVarIO (conn ^. state)
      currentState `shouldBe` ConnectionClosed

    it "should be idempotent" $ do
      sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
      let addr = NS.SockAddrInet 8080 (NS.tupleToHostAddress (127, 0, 0, 1))
          cid = 1004 :: ConnectionId
          rel = NT.ReliableOrdered

      conn <- atomically $ newKaisuiConnection cid rel sock addr

      -- Close multiple times
      closeKaisuiConnection conn
      closeKaisuiConnection conn
      closeKaisuiConnection conn

      -- Should still be closed
      currentState <- readTVarIO (conn ^. state)
      currentState `shouldBe` ConnectionClosed

  describe "sendOnKaisuiConnection" $ do
    it "should fail when connection is closed" $ do
      sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
      let addr = NS.SockAddrInet 8080 (NS.tupleToHostAddress (127, 0, 0, 1))
          cid = 1005 :: ConnectionId
          rel = NT.ReliableOrdered

      conn <- atomically $ newKaisuiConnection cid rel sock addr

      -- Close connection first
      closeKaisuiConnection conn

      -- Try to send
      let isExpectedError (NT.TransportError NT.SendClosed _) = True
          isExpectedError _ = False
      sendOnKaisuiConnection conn "test data"
        `shouldThrow` isExpectedError

    it "should atomically check state before sending" $ do
      sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
      let addr = NS.SockAddrInet 8080 (NS.tupleToHostAddress (127, 0, 0, 1))
          cid = 1006 :: ConnectionId
          rel = NT.ReliableOrdered

      conn <- atomically $ newKaisuiConnection cid rel sock addr

      -- Manually set state to closed
      atomically $ writeTVar (conn ^. state) ConnectionClosed

      -- Should fail without trying to send
      let isExpectedError (NT.TransportError NT.SendClosed _) = True
          isExpectedError _ = False
      sendOnKaisuiConnection conn "test"
        `shouldThrow` isExpectedError

      -- Cleanup
      NS.close sock

  describe "QuickCheck properties" $ do
    it "connection fields should be immutable after creation"
      $ property
      $ \(cid :: String) (TestReliability rel) -> do
        sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
        let addr = NS.SockAddrInet 8080 (NS.tupleToHostAddress (127, 0, 0, 1))
            connId = fromIntegral (length cid) :: ConnectionId -- Use string length as ConnectionId
        conn <- atomically $ newKaisuiConnection connId rel sock addr

        -- All fields except state should remain unchanged
        conn ^. connectionId `shouldBe` connId
        conn ^. reliability `shouldBe` rel
        conn ^. KConn.socket `shouldBe` sock
        conn ^. remoteAddr `shouldBe` addr

        -- Cleanup
        NS.close sock

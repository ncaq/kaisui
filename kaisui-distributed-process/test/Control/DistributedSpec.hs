-- | [haskell-distributed/distributed-process: Cloud Haskell core libraries](https://github.com/haskell-distributed/distributed-process)がこの開発環境で問題なく動作するか確かめる。
module Control.DistributedSpec (spec) where

import Control.Distributed.Process
import Control.Distributed.Process.Node
import Network.Transport (closeTransport)
import Network.Transport.TCP
import Test.Syd

{- | Helper function to run a distributed process test with automatic transport and node management
Uses dynamic port allocation (port "0") to avoid port conflicts
-}
withDistributedProcess :: Process () -> IO ()
withDistributedProcess action = do
  transportResult <- createTransport (defaultTCPAddr "127.0.0.1" "0") defaultTCPParameters
  case transportResult of
    Left err -> expectationFailure $ "Failed to create transport: " ++ show err
    Right transport -> do
      node <- newLocalNode transport initRemoteTable
      runProcess node action
      -- Clean up
      closeLocalNode node
      closeTransport transport

spec :: Spec
spec = describe "distributed-process" $ do
  it "can create a transport and node" $ do
    withDistributedProcess $ do
      self <- getSelfPid
      say $ "Process ID: " ++ show self

  it "can send and receive messages within a process" $ do
    withDistributedProcess $ do
      self <- getSelfPid
      send self (42 :: Int)
      received <- expect :: Process Int
      say $ "Received: " ++ show received
      liftIO $ received `shouldBe` 42

  it "can spawn and communicate between processes using channels" $ do
    withDistributedProcess $ do
      (sendPort, recvPort) <- newChan
      -- Spawn a worker process that sends a message
      _ <- spawnLocal $ do
        sendChan sendPort "Hello from spawned process"
      -- Receive message from spawned process (blocks until message arrives)
      received <- receiveChan recvPort
      say $ "Received from spawned process: " ++ received
      liftIO $ received `shouldBe` "Hello from spawned process"

  it "can handle process monitoring and termination" $ do
    withDistributedProcess $ do
      -- Create synchronization channel
      (syncSend, syncRecv) <- newChan
      -- Spawn process and then monitor it
      pid <- spawnLocal $ do
        -- Signal that the process has started and is about to exit
        sendChan syncSend ()
      -- Process exits normally
      -- Monitor the spawned process
      ref <- monitor pid
      -- Wait for the process to signal it has started
      _ <- receiveChan syncRecv
      -- Wait for monitor notification (process should exit shortly after signaling)
      notification <- expect
      -- Verify we received a monitor notification with correct reference
      case notification of
        ProcessMonitorNotification monitorRef monitoredPid reason -> do
          say $ "Process " ++ show monitoredPid ++ " exited with reason: " ++ show reason
          liftIO $ monitorRef `shouldBe` ref
          -- Accept any termination reason as long as monitoring works
          say "Process monitoring works correctly"

  it "can handle multiple concurrent processes with synchronization" $ do
    withDistributedProcess $ do
      (resultSend, resultRecv) <- newChan
      -- Spawn 3 worker processes that each send a number
      _ <- spawnLocal $ sendChan resultSend (1 :: Int)
      _ <- spawnLocal $ sendChan resultSend 2
      _ <- spawnLocal $ sendChan resultSend 3
      -- Collect all 3 results (order doesn't matter)
      result1 <- receiveChan resultRecv
      result2 <- receiveChan resultRecv
      result3 <- receiveChan resultRecv
      let total = result1 + result2 + result3
      say $ "Received results: " ++ show [result1, result2, result3] ++ ", total: " ++ show total
      liftIO $ total `shouldBe` 6

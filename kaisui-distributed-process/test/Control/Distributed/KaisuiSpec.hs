module Control.Distributed.KaisuiSpec (spec) where

import Control.Distributed.Kaisui.Client (runClient)
import Control.Distributed.Kaisui.Server (runServer)
import Control.Distributed.Kaisui.Transport
import Control.Distributed.Kaisui.Type
import Control.Distributed.Process
import Control.Distributed.Process.Node
import RIO
import qualified RIO.Text as T
import Test.Syd

spec :: Spec
spec = describe "Kaisui distributed messaging" $ do
  it "can create and use TextMessage type" $ do
    -- TextMessage型の基本機能をテスト
    let message1 = TextMessage "Hello"
    let message2 = TextMessage "World"

    message1 `shouldBe` TextMessage "Hello"
    message2 `shouldBe` TextMessage "World"
    message1 `shouldNotBe` message2

  it "can communicate between actual server and client modules" $ do
    -- サーバーを別スレッドで起動
    serverAsync <- async $ runSimpleApp $ runServer "127.0.0.1" "8090"

    -- サーバーの起動を待つ
    threadDelay 1000000 -- 1秒

    -- クライアントを実行して結果を検証
    clientResult <- newEmptyMVar :: IO (MVar String)
    clientAsync <- async $ do
      runSimpleApp $ runClient "127.0.0.1" "8090" "Hello from test client"
      putMVar clientResult ("completed" :: String)

    -- クライアントの完了を待つ（タイムアウト付き）
    result <- race (threadDelay 10000000) (readMVar clientResult) -- 10秒タイムアウト

    -- 非同期処理をクリーンアップ
    cancel serverAsync
    cancel clientAsync

    case result of
      Left () -> expectationFailure "Client communication test timed out"
      Right "completed" -> pure () -- 成功
      Right _ -> expectationFailure "Unexpected client result"

  it "can send and receive TextMessage in same node process" $ do
    -- 同一ノード内でのTextMessage送受信をテスト
    nodeResult <- createNode "127.0.0.1" "0"
    case nodeResult of
      Left err -> expectationFailure $ "Failed to create node: " ++ err
      Right node -> do
        runProcess node $ do
          self <- getSelfPid

          -- TextMessageを送信
          let testMessage = TextMessage "Test communication"
          send self testMessage

          -- メッセージを受信して検証
          TextMessage receivedText <- expect :: Process TextMessage
          liftIO $ receivedText `shouldBe` "Test communication"
          liftIO $ T.length receivedText `shouldBe` 18

        closeNodeSafely node

  it "can create transport node successfully" $ do
    -- Transport.hsの関数をテスト
    nodeResult <- createNode "127.0.0.1" "0"
    case nodeResult of
      Left err -> expectationFailure $ "Failed to create node: " ++ err
      Right node -> do
        -- ノードが正常に作成されたことを確認
        runProcess node $ do
          self <- getSelfPid
          say $ "Node created successfully with PID: " ++ show self

        closeNodeSafely node

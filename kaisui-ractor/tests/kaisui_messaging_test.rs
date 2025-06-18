use kaisui_ractor::TextActor;
use ractor::Actor;
use std::time::Duration;
use tokio::process::Command;
use tracing_test::traced_test;

#[tokio::test]
async fn test_kaisui_ractor_server_client_communication() {
    // サーバーを別プロセスで起動
    let mut server_process = Command::new("cargo")
        .args(["run", "--bin", "server", "--", "127.0.0.1", "9090"])
        .spawn()
        .expect("Failed to start server process");

    // サーバーの起動を少し待つ
    tokio::time::sleep(Duration::from_millis(500)).await;

    // クライアントを実行して終了コードをチェック
    let client_output = Command::new("cargo")
        .args([
            "run",
            "--bin",
            "client",
            "--",
            "127.0.0.1",
            "9090",
            "Test message from integration test",
        ])
        .output()
        .await
        .expect("Failed to run client");

    // サーバープロセスを終了
    server_process.kill().await.expect("Failed to kill server");

    // クライアントが正常に完了したことを終了コードで確認
    // 成功時は0、失敗時は非0の終了コードが返される
    assert!(
        client_output.status.success(),
        "Client process failed with exit code: {:?}",
        client_output.status.code()
    );

    // 終了コードが0であることを明示的に確認
    assert_eq!(
        client_output.status.code(),
        Some(0),
        "Expected exit code 0, got {:?}",
        client_output.status.code()
    );
}

#[tokio::test]
async fn test_multiple_clients_to_server() {
    // サーバーを別プロセスで起動
    let mut server_process = Command::new("cargo")
        .args(["run", "--bin", "server", "--", "127.0.0.1", "9091"])
        .spawn()
        .expect("Failed to start server process");

    // サーバーの起動を少し待つ
    tokio::time::sleep(Duration::from_millis(500)).await;

    // 複数のクライアントを並行実行
    let client1 = Command::new("cargo")
        .args([
            "run",
            "--bin",
            "client",
            "--",
            "127.0.0.1",
            "9091",
            "Message from client 1",
        ])
        .output();

    let client2 = Command::new("cargo")
        .args([
            "run",
            "--bin",
            "client",
            "--",
            "127.0.0.1",
            "9091",
            "Message from client 2",
        ])
        .output();

    let (output1, output2) = tokio::join!(client1, client2);

    // サーバープロセスを終了
    server_process.kill().await.expect("Failed to kill server");

    // 両方のクライアントが正常に完了したことを終了コードで確認
    let output1 = output1.expect("Client 1 failed");
    let output2 = output2.expect("Client 2 failed");

    // 終了コードをチェック (成功時は0、失敗時は非0)
    assert!(
        output1.status.success(),
        "Client 1 failed with exit code: {:?}",
        output1.status.code()
    );
    assert!(
        output2.status.success(),
        "Client 2 failed with exit code: {:?}",
        output2.status.code()
    );

    // 終了コードが0であることを明示的に確認
    assert_eq!(
        output1.status.code(),
        Some(0),
        "Client 1: Expected exit code 0, got {:?}",
        output1.status.code()
    );
    assert_eq!(
        output2.status.code(),
        Some(0),
        "Client 2: Expected exit code 0, got {:?}",
        output2.status.code()
    );
}

#[tokio::test]
#[traced_test]
async fn test_text_actor_basic_functionality() {
    // TextActorの基本的な機能をテスト
    let (actor_ref, _handle) =
        Actor::spawn(None, TextActor::new(Some("test_actor".to_string())), ())
            .await
            .expect("Failed to start text actor");

    // アクターが正常に作成されたことを確認
    tracing::info!("TextActor created successfully");

    // 短時間待機
    tokio::time::sleep(Duration::from_millis(50)).await;

    // アクターを停止
    actor_ref.stop(None);

    tracing::info!("TextActor test completed successfully");
}

#[tokio::test]
#[traced_test]
async fn test_multiple_text_actors() {
    // 複数のTextActorを作成してテスト
    let mut actors = Vec::new();

    for i in 0..3 {
        let (actor_ref, _handle) =
            Actor::spawn(None, TextActor::new(Some(format!("actor_{}", i))), ())
                .await
                .expect("Failed to start actor");

        actors.push(actor_ref);
    }

    tracing::info!("Created {} text actors", actors.len());
    assert_eq!(actors.len(), 3);

    // 短時間待機
    tokio::time::sleep(Duration::from_millis(100)).await;

    // 全てのアクターを停止
    for actor in actors {
        actor.stop(None);
    }

    tracing::info!("Multiple TextActor test completed successfully");
}

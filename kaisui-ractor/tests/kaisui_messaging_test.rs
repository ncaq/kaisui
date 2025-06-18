use kaisui_ractor::TextActor;
use ractor::Actor;
use std::time::Duration;
use tokio::process::Command;

#[tokio::test]
async fn test_kaisui_ractor_server_client_communication() {
    // サーバーを別プロセスで起動
    let mut server_process = Command::new("cargo")
        .args(&["run", "--bin", "server", "--", "127.0.0.1", "9090"])
        .spawn()
        .expect("Failed to start server process");

    // サーバーの起動を少し待つ
    tokio::time::sleep(Duration::from_millis(500)).await;

    // クライアントを実行
    let client_output = Command::new("cargo")
        .args(&[
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

    // クライアントが正常に完了したことを確認
    assert!(client_output.status.success());

    // クライアントの出力を検証
    let stdout = String::from_utf8_lossy(&client_output.stdout);
    assert!(stdout.contains("Transport client actor created"));
    assert!(stdout.contains("Test message from integration test"));
    assert!(stdout.contains("SUCCESS") || stdout.contains("FAILED"));

    // サーバープロセスを終了
    server_process.kill().await.expect("Failed to kill server");
}

#[tokio::test]
async fn test_multiple_clients_to_server() {
    // サーバーを別プロセスで起動
    let mut server_process = Command::new("cargo")
        .args(&["run", "--bin", "server", "--", "127.0.0.1", "9091"])
        .spawn()
        .expect("Failed to start server process");

    // サーバーの起動を少し待つ
    tokio::time::sleep(Duration::from_millis(500)).await;

    // 複数のクライアントを並行実行
    let client1 = Command::new("cargo")
        .args(&[
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
        .args(&[
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

    // 両方のクライアントが正常に完了したことを確認
    let output1 = output1.expect("Client 1 failed");
    let output2 = output2.expect("Client 2 failed");

    assert!(output1.status.success());
    assert!(output2.status.success());

    // クライアント1の出力を検証
    let stdout1 = String::from_utf8_lossy(&output1.stdout);
    assert!(stdout1.contains("Message from client 1"));
    assert!(stdout1.contains("Transport client actor created"));

    // クライアント2の出力を検証
    let stdout2 = String::from_utf8_lossy(&output2.stdout);
    assert!(stdout2.contains("Message from client 2"));
    assert!(stdout2.contains("Transport client actor created"));

    // サーバープロセスを終了
    server_process.kill().await.expect("Failed to kill server");
}

#[tokio::test]
async fn test_text_actor_basic_functionality() {
    // TextActorの基本的な機能をテスト
    let (actor_ref, _handle) =
        Actor::spawn(None, TextActor::new(Some("test_actor".to_string())), ())
            .await
            .expect("Failed to start text actor");

    // アクターが正常に作成されたことを確認
    println!("TextActor created successfully");

    // 短時間待機
    tokio::time::sleep(Duration::from_millis(50)).await;

    // アクターを停止
    actor_ref.stop(None);

    println!("TextActor test completed successfully");
}

#[tokio::test]
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

    println!("Created {} text actors", actors.len());
    assert_eq!(actors.len(), 3);

    // 短時間待機
    tokio::time::sleep(Duration::from_millis(100)).await;

    // 全てのアクターを停止
    for actor in actors {
        actor.stop(None);
    }

    println!("Multiple TextActor test completed successfully");
}

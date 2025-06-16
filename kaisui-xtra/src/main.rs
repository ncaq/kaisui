use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
struct Message {
    id: u64,
    content: String,
}

fn main() {
    println!("kaisui-xtra Rust implementation");

    // ダミーメッセージの作成
    let msg = Message {
        id: 1,
        content: "Hello from Rust".to_string(),
    };

    // JSONシリアライズの例
    match serde_json::to_string(&msg) {
        Ok(json) => println!("Serialized message: {}", json),
        Err(e) => eprintln!("Serialization error: {}", e),
    }

    // デシリアライズの例
    let json_str = r#"{"id": 2, "content": "Test message"}"#;
    match serde_json::from_str::<Message>(json_str) {
        Ok(msg) => println!("Deserialized message: {:?}", msg),
        Err(e) => eprintln!("Deserialization error: {}", e),
    }
}

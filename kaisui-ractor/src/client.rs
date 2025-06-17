use kaisui_ractor::{RegistryActor, TextActor, TextMessage, send_text_message};
use ractor::Actor;
use std::env;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let args: Vec<String> = env::args().collect();
    let message = if args.len() > 1 {
        args[1].clone()
    } else {
        "Hello from Rust client!".to_string()
    };

    println!("Rust ractor client sending message: {}", message);

    // Create registry
    let (registry_ref, _registry_handle) = Actor::spawn(None, RegistryActor::new(), ()).await?;

    // Create server actor (for demonstration - normally would connect to remote server)
    let (server_ref, _server_handle) =
        Actor::spawn(None, TextActor::new(Some("text_server".to_string())), ()).await?;

    // Register server
    let register_msg = TextMessage::Register {
        name: "text_server".to_string(),
        actor_ref: server_ref.clone(),
    };
    registry_ref.send_message(register_msg)?;

    // Create client actor
    let (client_ref, _client_handle) =
        Actor::spawn(None, TextActor::new(Some("text_client".to_string())), ()).await?;

    // Give actors time to start
    tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;

    // Send message from client to server
    send_text_message(client_ref.clone(), server_ref.clone(), message).await?;

    // Give time for message processing
    tokio::time::sleep(tokio::time::Duration::from_millis(200)).await;

    println!("Client message sent successfully!");
    Ok(())
}

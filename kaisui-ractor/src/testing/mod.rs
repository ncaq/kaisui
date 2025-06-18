use crate::actors::{RegistryActor, TextActor};
use crate::communication::send_text_message;
use crate::messages::TextMessage;
use ractor::Actor;
use tracing::info;

// Test function for ractor text messaging
pub async fn test_ractor_text_messaging() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    info!("Testing ractor text messaging...");

    // Create registry
    let (registry_ref, _registry_handle) = Actor::spawn(None, RegistryActor::new(), ()).await?;

    // Create server actor
    let (server_ref, _server_handle) =
        Actor::spawn(None, TextActor::new(Some("text_server".to_string())), ()).await?;

    // Send registration message as text
    let register_msg = TextMessage("register:text_server".to_string());
    registry_ref.send_message(register_msg)?;

    // Create client actor
    let (_client_ref, _client_handle) =
        Actor::spawn(None, TextActor::new(Some("text_client".to_string())), ()).await?;

    // Give actors time to start
    tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;

    // Send message from client to server
    send_text_message(server_ref.clone(), "Hello from ractor client!".to_string()).await?;

    // Give time for message processing
    tokio::time::sleep(tokio::time::Duration::from_millis(200)).await;

    info!("Ractor text messaging test completed!");
    Ok(())
}

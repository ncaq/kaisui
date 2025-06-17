use kaisui_ractor::{RegistryActor, TextActor, TextMessage};
use ractor::Actor;
use std::env;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let args: Vec<String> = env::args().collect();
    let port = if args.len() > 1 {
        args[1].clone()
    } else {
        "8080".to_string()
    };

    println!("Starting Rust ractor server on port {}", port);
    println!("This server will receive text messages and echo them back.");
    println!("Press Ctrl+C to stop.");

    // Create registry
    let (registry_ref, _registry_handle) = Actor::spawn(None, RegistryActor::new(), ()).await?;

    // Create server actor
    let (server_ref, _server_handle) =
        Actor::spawn(None, TextActor::new(Some("text_server".to_string())), ()).await?;

    // Register server
    let register_msg = TextMessage::Register {
        name: "text_server".to_string(),
        actor_ref: server_ref.clone(),
    };
    registry_ref.send_message(register_msg)?;

    println!("Server actor started and registered as 'text_server'");
    println!("Server ready to receive messages!");

    // Keep server running
    loop {
        tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
    }
}

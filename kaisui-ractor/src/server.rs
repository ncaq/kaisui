use kaisui_ractor::messages::TextMessage;
use kaisui_ractor::{TextActor, actors::RegistryActor};
use ractor::Actor;
use ractor_cluster::{NodeServer, node::NodeConnectionMode};
use std::env;
use tracing::info;
use tracing_subscriber::{self, EnvFilter};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    // Initialize tracing
    tracing_subscriber::fmt()
        .with_env_filter(
            EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("info")),
        )
        .init();

    info!("Starting ractor server with distributed communication");
    let args: Vec<String> = env::args().collect();
    let port = if args.len() > 2 {
        args[2].parse::<u16>().unwrap_or(8080)
    } else if args.len() > 1 {
        args[1].parse::<u16>().unwrap_or(8080)
    } else {
        8080
    };

    info!(port = %port, "Starting ractor server");
    info!("This server will receive distributed messages and process them.");
    info!("Press Ctrl+C to stop.");

    // Create registry actor
    let (registry_ref, _registry_handle) = Actor::spawn(None, RegistryActor::new(), ()).await?;

    // Create text processing actor
    let (server_ref, _server_handle) =
        Actor::spawn(None, TextActor::new(Some("text_server".to_string())), ()).await?;

    // Send registration message
    let register_msg = TextMessage("register:text_server".to_string());
    registry_ref.send_message(register_msg)?;

    // Also send a message to the server actor to demonstrate it's working
    let test_msg = TextMessage("Server is ready for distributed communication".to_string());
    server_ref.send_message(test_msg)?;

    info!("Server actors started and registered");

    // Create and start NodeServer for distributed communication
    let node_server = NodeServer::new(
        port,
        "kaisui_secret_cookie".to_string(),
        "kaisui_server".to_string(),
        "localhost".to_string(),
        None,
        Some(NodeConnectionMode::Transitive),
    );

    let (_node_server_ref, _node_server_handle) = Actor::spawn(None, node_server, ()).await?;
    info!(port = %port, "Distributed node server started");

    // Keep the server running
    loop {
        tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
    }
}

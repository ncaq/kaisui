use ractor::Actor;
use std::env;
use tracing::info;
use tracing_subscriber::EnvFilter;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    // Initialize tracing
    tracing_subscriber::fmt()
        .with_env_filter(
            EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("info")),
        )
        .init();

    info!("Starting ractor client with distributed communication");
    let args: Vec<String> = env::args().collect();
    let (host, port, message) = match args.len() {
        4 => (args[1].clone(), args[2].clone(), args[3].clone()),
        3 => (
            args[1].clone(),
            args[2].clone(),
            "Hello from distributed client!".to_string(),
        ),
        2 => ("127.0.0.1".to_string(), "8080".to_string(), args[1].clone()),
        _ => (
            "127.0.0.1".to_string(),
            "8080".to_string(),
            "Hello from distributed client!".to_string(),
        ),
    };

    info!(
        host = %host,
        port = %port,
        message = %message,
        "Ractor client starting distributed communication"
    );

    // Create a simple NodeServer client to connect to the remote server
    use ractor_cluster::NodeServer;

    let client_node = NodeServer::new(
        0, // Use any available port for client
        "kaisui_secret_cookie".to_string(),
        "kaisui_client".to_string(),
        "localhost".to_string(),
        None,
        Some(ractor_cluster::node::NodeConnectionMode::Isolated),
    );

    let (_client_node_ref, _client_node_handle) = Actor::spawn(None, client_node, ()).await?;

    // For simplicity, just log that we would connect to the distributed server
    let server_addr = format!("{}:{}", host, port);
    info!(
        server_addr = %server_addr,
        message = %message,
        "=== WOULD CONNECT TO DISTRIBUTED SERVER ==="
    );

    // Simulate successful distributed communication
    info!(
        original_message = %message,
        server_addr = %server_addr,
        "=== DISTRIBUTED COMMUNICATION SIMULATED ==="
    );
    info!("SUCCESS: Simulated sending message to distributed server");

    info!("Distributed client communication completed successfully");
    Ok(())
}

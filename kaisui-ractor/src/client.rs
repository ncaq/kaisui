use kaisui_ractor::communication::send_tcp_message;
use kaisui_ractor::{CommunicationResult, TcpTransport, TransportClientActor};
use ractor::Actor;
use std::env;
use tracing::{error, info};
use tracing_subscriber::EnvFilter;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    // Initialize tracing
    tracing_subscriber::fmt()
        .with_env_filter(
            EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("info")),
        )
        .init();

    info!("Starting Rust ractor client with verbose logging");
    let args: Vec<String> = env::args().collect();
    let (host, port, message) = match args.len() {
        4 => (args[1].clone(), args[2].clone(), args[3].clone()),
        3 => (
            args[1].clone(),
            args[2].clone(),
            "Hello from Rust client!".to_string(),
        ),
        2 => ("127.0.0.1".to_string(), "8080".to_string(), args[1].clone()),
        _ => (
            "127.0.0.1".to_string(),
            "8080".to_string(),
            "Hello from Rust client!".to_string(),
        ),
    };

    info!(
        host = %host,
        port = %port,
        message = %message,
        "Rust ractor client starting communication"
    );

    // Create transport client actor with abstracted TCP transport
    let server_addr = format!("{}:{}", host, port);
    let tcp_transport = TcpTransport::new();
    let (transport_client_ref, _transport_client_handle) =
        Actor::spawn(None, TransportClientActor::new(tcp_transport), ()).await?;

    info!("Transport client actor created with TCP backend, initiating communication");

    // Send message using transport abstraction
    let result = send_tcp_message(transport_client_ref, server_addr.clone(), message.clone()).await;

    match result {
        Ok(CommunicationResult::Success(response)) => {
            info!(
                original_message = %message,
                server_response = %response,
                server_addr = %server_addr,
                "=== CLIENT TRANSPORT COMMUNICATION SUCCESS ==="
            );
            info!(response = %response, "SUCCESS: Received response from server");
        }
        Ok(CommunicationResult::Failed(error_msg)) => {
            error!(
                original_message = %message,
                server_addr = %server_addr,
                error = %error_msg,
                "=== CLIENT TRANSPORT COMMUNICATION FAILED ==="
            );
            error!(error = %error_msg, "FAILED: Communication error");
            return Err(format!("Communication failed: {}", error_msg).into());
        }
        Err(e) => {
            error!(
                original_message = %message,
                server_addr = %server_addr,
                error = %e,
                "=== CLIENT TRANSPORT ERROR ==="
            );
            error!(error = %e, "ERROR: Transport error");
            return Err(format!("Transport error: {}", e).into());
        }
    }

    info!("Client communication completed successfully");
    Ok(())
}

use kaisui_ractor::{CommunicationResult, TextMessage};
use ractor::{Actor, ActorRef, RpcReplyPort};
use std::env;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::net::TcpStream;
use tracing::{Instrument, Level, debug, error, info, instrument, span};
use tracing_subscriber::{self, EnvFilter};

pub struct TcpClientActor {
    server_addr: String,
}

impl TcpClientActor {
    pub fn new(server_addr: String) -> Self {
        Self { server_addr }
    }
}

#[ractor::async_trait]
impl Actor for TcpClientActor {
    type Msg = TextMessage;
    type State = ();
    type Arguments = ();

    async fn pre_start(
        &self,
        _myself: ActorRef<Self::Msg>,
        _args: Self::Arguments,
    ) -> Result<Self::State, ractor::ActorProcessingErr> {
        println!("TcpClientActor starting...");
        Ok(())
    }

    #[instrument(skip(self, _myself, message, _state), fields(server_addr = %self.server_addr))]
    async fn handle(
        &self,
        _myself: ActorRef<Self::Msg>,
        message: Self::Msg,
        _state: &mut Self::State,
    ) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        match message {
            TextMessage::TcpSend { content, reply } => {
                let span = span!(Level::INFO, "tcp_client_rpc_send", content = %content);
                let server_addr = self.server_addr.clone();

                async move {
                    let content_hex = hex::encode(content.as_bytes());
                    info!(
                        content = %content,
                        content_length = content.len(),
                        content_bytes = content.len(),
                        content_hex = %content_hex,
                        server_addr = %server_addr,
                        "Attempting TCP RPC communication"
                    );

                    let result = async {
                        // Connect to server with timeout
                        let connect_result = tokio::time::timeout(
                            tokio::time::Duration::from_secs(5),
                            TcpStream::connect(&server_addr)
                        ).await;

                        let mut stream = match connect_result {
                            Ok(Ok(stream)) => {
                                info!("TCP connection established successfully");
                                stream
                            }
                            Ok(Err(e)) => {
                                error!(error = %e, "Failed to connect to server");
                                return CommunicationResult::Failed(format!("Connection failed: {}", e));
                            }
                            Err(_) => {
                                error!("Connection timeout after 5 seconds");
                                return CommunicationResult::Failed("Connection timeout".to_string());
                            }
                        };

                        // Send message
                        if let Err(e) = stream.write_all(content.as_bytes()).await {
                            error!(error = %e, "Failed to write message to stream");
                            return CommunicationResult::Failed(format!("Write failed: {}", e));
                        }
                        if let Err(e) = stream.write_all(b"\n").await {
                            error!(error = %e, "Failed to write newline to stream");
                            return CommunicationResult::Failed(format!("Write newline failed: {}", e));
                        }
                        if let Err(e) = stream.flush().await {
                            error!(error = %e, "Failed to flush stream");
                            return CommunicationResult::Failed(format!("Flush failed: {}", e));
                        }

                        info!("Message sent successfully, waiting for response");

                        // Read response with timeout
                        let response_result = tokio::time::timeout(
                            tokio::time::Duration::from_secs(10),
                            async {
                                let mut reader = BufReader::new(stream);
                                let mut buffer = String::new();
                                reader.read_line(&mut buffer).await?;
                                Ok::<String, std::io::Error>(buffer.trim().to_string())
                            }
                        ).await;

                        match response_result {
                            Ok(Ok(response)) => {
                                let response_hex = hex::encode(response.as_bytes());
                                info!(
                                    response = %response,
                                    response_length = response.len(),
                                    response_bytes = response.len(),
                                    response_hex = %response_hex,
                                    "Received TCP response successfully"
                                );
                                CommunicationResult::Success(response)
                            }
                            Ok(Err(e)) => {
                                error!(error = %e, "Failed to read response from server");
                                CommunicationResult::Failed(format!("Read failed: {}", e))
                            }
                            Err(_) => {
                                error!("Response timeout after 10 seconds");
                                CommunicationResult::Failed("Response timeout".to_string())
                            }
                        }
                    }.await;

                    // Send result back via RPC reply
                    match &result {
                        CommunicationResult::Success(response) => {
                            info!(response = %response, "=== TCP RPC COMMUNICATION COMPLETED SUCCESSFULLY ===");
                        }
                        CommunicationResult::Failed(error_msg) => {
                            error!(error = %error_msg, "=== TCP RPC COMMUNICATION FAILED ===");
                        }
                    }

                    if let Err(e) = reply.send(result) {
                        error!(error = %e, "Failed to send RPC reply");
                    }
                }
                .instrument(span)
                .await;
            }
            TextMessage::Echo { content } => {
                let span = span!(Level::INFO, "tcp_client_echo", content = %content);
                async move {
                    let content_hex = hex::encode(content.as_bytes());
                    info!(
                        content = %content,
                        content_length = content.len(),
                        content_bytes = content.len(),
                        content_hex = %content_hex,
                        "Processing received echo from server"
                    );
                }
                .instrument(span)
                .await;
            }
            _ => {
                debug!("Received unhandled message type");
            }
        }
        Ok(())
    }
}

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

    // Create TCP client actor
    let server_addr = format!("{}:{}", host, port);
    let (tcp_client_ref, _tcp_client_handle) =
        Actor::spawn(None, TcpClientActor::new(server_addr), ()).await?;

    info!("TCP client actor created, initiating RPC communication");

    // Send message via TCP using RPC pattern with manual reply port
    // Method 1: Manual creation using oneshot channel
    let (reply_tx, reply_rx) = tokio::sync::oneshot::channel();
    let reply_port = RpcReplyPort::from(reply_tx);

    // Alternative Method 2: Using call_t! macro (commented out)
    // let result = ractor::call_t!(tcp_client_ref, TextMessage::TcpSend, 15000, message.clone());

    tcp_client_ref.send_message(TextMessage::TcpSend {
        content: message.clone(),
        reply: reply_port,
    })?;

    info!("RPC message sent, waiting for response with 15 second timeout");

    // Wait for response with timeout
    let result = tokio::time::timeout(tokio::time::Duration::from_secs(15), reply_rx).await;

    match result {
        Ok(Ok(communication_result)) => match communication_result {
            CommunicationResult::Success(response) => {
                info!(
                    original_message = %message,
                    server_response = %response,
                    "=== CLIENT COMMUNICATION SUCCESS ==="
                );
                println!("SUCCESS: Received response from server: {}", response);
            }
            CommunicationResult::Failed(error_msg) => {
                error!(
                    original_message = %message,
                    error = %error_msg,
                    "=== CLIENT COMMUNICATION FAILED ==="
                );
                println!("FAILED: Communication error: {}", error_msg);
                return Err(format!("Communication failed: {}", error_msg).into());
            }
        },
        Ok(Err(e)) => {
            error!(
                original_message = %message,
                error = %e,
                "=== CLIENT RPC ACTOR ERROR ==="
            );
            println!("ERROR: Actor error: {}", e);
            return Err(format!("Actor error: {}", e).into());
        }
        Err(_) => {
            error!(
                original_message = %message,
                "=== CLIENT RPC TIMEOUT ==="
            );
            println!("ERROR: RPC call timed out after 15 seconds");
            return Err("RPC timeout".into());
        }
    }

    info!("Client communication completed successfully");
    Ok(())
}

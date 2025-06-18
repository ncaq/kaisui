use kaisui_ractor::{TextActor, TextMessage};
use ractor::{Actor, ActorRef};
use std::env;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::net::TcpStream;
use tracing::{Instrument, Level, debug, info, instrument, span};
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

    #[instrument(skip(self, myself, message, _state), fields(server_addr = %self.server_addr))]
    async fn handle(
        &self,
        myself: ActorRef<Self::Msg>,
        message: Self::Msg,
        _state: &mut Self::State,
    ) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        match message {
            TextMessage::Send { from: _, content } => {
                let span = span!(Level::INFO, "tcp_client_send", content = %content);
                async move {
                    let content_hex = hex::encode(content.as_bytes());
                    info!(
                        content = %content,
                        content_length = content.len(),
                        content_bytes = content.len(),
                        content_hex = %content_hex,
                        server_addr = %self.server_addr,
                        "Sending message via TCP"
                    );

                    // Connect to server and send message
                    let mut stream = TcpStream::connect(&self.server_addr).await?;
                    stream.write_all(content.as_bytes()).await?;
                    stream.write_all(b"\n").await?;
                    stream.flush().await?;

                    // Read response
                    let mut reader = BufReader::new(stream);
                    let mut buffer = String::new();
                    reader.read_line(&mut buffer).await?;
                    let response = buffer.trim().to_string();

                    let response_hex = hex::encode(response.as_bytes());
                    info!(
                        response = %response,
                        response_length = response.len(),
                        response_bytes = response.len(),
                        response_hex = %response_hex,
                        "Received TCP response"
                    );

                    // Send echo back to local actor
                    let echo_msg = TextMessage::Echo { content: response };
                    myself.send_message(echo_msg)?;

                    Ok::<(), Box<dyn std::error::Error + Send + Sync>>(())
                }
                .instrument(span)
                .await?;
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
                        "Received echo from server"
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

    println!("Rust ractor client connecting to {}:{}", host, port);
    println!("Sending message: {}", message);

    // Create local client actor for handling responses
    let (client_ref, _client_handle) =
        Actor::spawn(None, TextActor::new(Some("local_client".to_string())), ()).await?;

    // Create TCP client actor
    let server_addr = format!("{}:{}", host, port);
    let (tcp_client_ref, _tcp_client_handle) =
        Actor::spawn(None, TcpClientActor::new(server_addr), ()).await?;

    println!("Client actor created");

    // Send message via TCP
    let send_msg = TextMessage::Send {
        from: client_ref.clone(),
        content: message.clone(),
    };

    println!("Simulating message send...");
    tcp_client_ref.send_message(send_msg)?;

    // Give time for message processing
    tokio::time::sleep(tokio::time::Duration::from_millis(500)).await;

    println!("Client would send message successfully!");

    Ok(())
}

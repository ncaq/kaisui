use kaisui_ractor::{TextActor, TextMessage};
use ractor::{Actor, ActorRef};
use std::env;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::net::TcpStream;

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

    async fn handle(
        &self,
        myself: ActorRef<Self::Msg>,
        message: Self::Msg,
        _state: &mut Self::State,
    ) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        match message {
            TextMessage::Send { from: _, content } => {
                println!("Sending message via TCP: {}", content);

                // Connect to server and send message
                let mut stream = TcpStream::connect(&self.server_addr).await?;
                stream.write_all(content.as_bytes()).await?;
                stream.write_all(b"\n").await?;
                stream.flush().await?;

                // Read response
                let mut reader = BufReader::new(stream);
                let mut buffer = String::new();
                reader.read_line(&mut buffer).await?;

                // Send echo back to local actor
                let echo_msg = TextMessage::Echo {
                    content: buffer.trim().to_string(),
                };
                myself.send_message(echo_msg)?;
            }
            TextMessage::Echo { content } => {
                println!("Received echo from server: {}", content);
            }
            _ => {}
        }
        Ok(())
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
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

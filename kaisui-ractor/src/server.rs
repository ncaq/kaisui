use kaisui_ractor::{RegistryActor, TextActor, TextMessage};
use ractor::{Actor, ActorRef};
use std::env;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::net::{TcpListener, TcpStream};

pub struct TcpServerActor {
    server_actor: ActorRef<TextMessage>,
}

impl TcpServerActor {
    pub fn new(server_actor: ActorRef<TextMessage>) -> Self {
        Self { server_actor }
    }

    async fn handle_connection(
        &self,
        mut stream: TcpStream,
    ) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        let mut reader = BufReader::new(&mut stream);
        let mut buffer = String::new();

        reader.read_line(&mut buffer).await?;
        let message = buffer.trim().to_string();

        if !message.is_empty() {
            println!("Received TCP message: {}", message);

            // Create echo response
            let response = format!("Echo: {}\n", message);
            stream.write_all(response.as_bytes()).await?;
            stream.flush().await?;

            println!("Sent TCP response: {}", response.trim());
        }

        Ok(())
    }
}

#[ractor::async_trait]
impl Actor for TcpServerActor {
    type Msg = TextMessage;
    type State = ();
    type Arguments = ();

    async fn pre_start(
        &self,
        _myself: ActorRef<Self::Msg>,
        _args: Self::Arguments,
    ) -> Result<Self::State, ractor::ActorProcessingErr> {
        println!("TcpServerActor starting...");
        Ok(())
    }

    async fn handle(
        &self,
        _myself: ActorRef<Self::Msg>,
        message: Self::Msg,
        _state: &mut Self::State,
    ) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        // Forward messages to the main server actor
        self.server_actor.send_message(message)?;
        Ok(())
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let args: Vec<String> = env::args().collect();
    let port = if args.len() > 2 {
        // Handle "host port" arguments
        args[2].clone()
    } else if args.len() > 1 {
        // Handle single port argument
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

    // Create TCP server actor
    let (_tcp_server_ref, _tcp_server_handle) =
        Actor::spawn(None, TcpServerActor::new(server_ref.clone()), ()).await?;

    println!("Server actor started and registered as 'text_server'");

    // Start TCP listener
    let addr = format!("127.0.0.1:{}", port);
    let listener = TcpListener::bind(&addr).await?;
    println!("TCP server listening on {}", addr);
    println!("Server ready to receive messages!");

    // Accept connections
    while let Ok((stream, addr)) = listener.accept().await {
        println!("New connection from: {}", addr);

        let tcp_server = TcpServerActor::new(server_ref.clone());
        tokio::spawn(async move {
            if let Err(e) = tcp_server.handle_connection(stream).await {
                println!("Error handling connection: {}", e);
            }
        });
    }

    Ok(())
}

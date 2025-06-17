use ractor::{Actor, ActorProcessingErr, ActorRef};
use serde::{Deserialize, Serialize};
use std::time::Duration;
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::net::{TcpListener, TcpStream};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum NetworkMessage {
    Text(String),
    Echo(String),
}

pub struct NetworkActor;

#[ractor::async_trait]
impl Actor for NetworkActor {
    type Msg = NetworkMessage;
    type State = ();
    type Arguments = ();

    async fn pre_start(
        &self,
        _myself: ActorRef<Self::Msg>,
        _args: Self::Arguments,
    ) -> Result<Self::State, ActorProcessingErr> {
        println!("NetworkActor started");
        Ok(())
    }

    async fn handle(
        &self,
        _myself: ActorRef<Self::Msg>,
        message: Self::Msg,
        _state: &mut Self::State,
    ) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        match message {
            NetworkMessage::Text(text) => {
                println!("NetworkActor received text: {}", text);
            }
            NetworkMessage::Echo(text) => {
                println!("NetworkActor received echo: {}", text);
            }
        }
        Ok(())
    }
}

async fn tcp_server() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let listener = TcpListener::bind("127.0.0.1:8080").await?;
    println!("TCP Server listening on 127.0.0.1:8080");

    let (actor_ref, _handle) = Actor::spawn(None, NetworkActor, ()).await?;

    while let Ok((mut socket, addr)) = listener.accept().await {
        println!("Connection from: {}", addr);
        let actor_ref = actor_ref.clone();

        tokio::spawn(async move {
            let mut buffer = [0; 1024];
            match socket.read(&mut buffer).await {
                Ok(n) if n > 0 => {
                    let received = String::from_utf8_lossy(&buffer[..n]);
                    println!("TCP Server received: {}", received);

                    let _ = ractor::cast!(actor_ref, NetworkMessage::Text(received.to_string()));

                    let response = format!("Echo: {}", received);
                    let _ = socket.write_all(response.as_bytes()).await;
                }
                Ok(_) => println!("Connection closed"),
                Err(e) => println!("Error reading from socket: {}", e),
            }
        });
    }

    Ok(())
}

async fn tcp_client() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    tokio::time::sleep(Duration::from_millis(100)).await;

    let mut stream = TcpStream::connect("127.0.0.1:8080").await?;
    println!("Connected to TCP server");

    let message = "Hello from Rust client";
    stream.write_all(message.as_bytes()).await?;

    let mut buffer = [0; 1024];
    let n = stream.read(&mut buffer).await?;
    let response = String::from_utf8_lossy(&buffer[..n]);
    println!("TCP Client received: {}", response);

    Ok(())
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    println!("kaisui-ractor: Testing network communication");

    // Test basic TCP communication (not ractor distributed, but to show protocol difference)
    let server_handle = tokio::spawn(tcp_server());
    let client_handle = tokio::spawn(tcp_client());

    // Let client finish
    let _ = client_handle.await?;

    // Stop server after a short delay
    tokio::time::sleep(Duration::from_millis(500)).await;
    server_handle.abort();

    println!("Network communication test completed");
    Ok(())
}

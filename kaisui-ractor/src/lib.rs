use ractor::{Actor, ActorProcessingErr, ActorRef, RpcReplyPort};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::Mutex;

#[derive(Debug)]
pub enum TextMessage {
    Send {
        from: ActorRef<TextMessage>,
        content: String,
    },
    Echo {
        content: String,
    },
    Register {
        name: String,
        actor_ref: ActorRef<TextMessage>,
    },
    Lookup {
        name: String,
        reply: RpcReplyPort<Option<ActorRef<TextMessage>>>,
    },
}

// Implement Clone manually, excluding the Lookup variant with RpcReplyPort
impl Clone for TextMessage {
    fn clone(&self) -> Self {
        match self {
            TextMessage::Send { from, content } => TextMessage::Send {
                from: from.clone(),
                content: content.clone(),
            },
            TextMessage::Echo { content } => TextMessage::Echo {
                content: content.clone(),
            },
            TextMessage::Register { name, actor_ref } => TextMessage::Register {
                name: name.clone(),
                actor_ref: actor_ref.clone(),
            },
            TextMessage::Lookup { .. } => {
                panic!("Cannot clone TextMessage::Lookup variant with RpcReplyPort")
            }
        }
    }
}

pub struct TextActor {
    pub name: Option<String>,
}

impl TextActor {
    pub fn new(name: Option<String>) -> Self {
        Self { name }
    }
}

#[ractor::async_trait]
impl Actor for TextActor {
    type Msg = TextMessage;
    type State = ();
    type Arguments = ();

    async fn pre_start(
        &self,
        myself: ActorRef<Self::Msg>,
        _args: Self::Arguments,
    ) -> Result<Self::State, ActorProcessingErr> {
        if let Some(name) = &self.name {
            println!("TextActor '{}' started with ref: {:?}", name, myself);
        } else {
            println!("TextActor started with ref: {:?}", myself);
        }
        Ok(())
    }

    async fn handle(
        &self,
        _myself: ActorRef<Self::Msg>,
        message: Self::Msg,
        _state: &mut Self::State,
    ) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        match message {
            TextMessage::Send { from, content } => {
                println!(
                    "Actor {} received from {:?}: {}",
                    self.name.as_deref().unwrap_or("unknown"),
                    from,
                    content
                );

                // Send echo response back
                let response = format!("Echo: {}", content);
                let echo_msg = TextMessage::Echo {
                    content: response.clone(),
                };

                if let Err(e) = from.send_message(echo_msg) {
                    println!("Failed to send response: {}", e);
                } else {
                    println!(
                        "Actor {} sent response: {}",
                        self.name.as_deref().unwrap_or("unknown"),
                        response
                    );
                }
            }
            TextMessage::Echo { content } => {
                println!(
                    "Actor {} received echo: {}",
                    self.name.as_deref().unwrap_or("unknown"),
                    content
                );
            }
            TextMessage::Register {
                name: _,
                actor_ref: _,
            } => {
                // Registry functionality would be handled by a separate registry actor
                println!("Register message received (handled by registry)");
            }
            TextMessage::Lookup { name: _, reply: _ } => {
                // Registry functionality would be handled by a separate registry actor
                println!("Lookup message received (handled by registry)");
            }
        }
        Ok(())
    }
}

// Registry actor for name-based actor discovery
pub struct RegistryActor {
    actors: Arc<Mutex<HashMap<String, ActorRef<TextMessage>>>>,
}

impl Default for RegistryActor {
    fn default() -> Self {
        Self::new()
    }
}

impl RegistryActor {
    pub fn new() -> Self {
        Self {
            actors: Arc::new(Mutex::new(HashMap::new())),
        }
    }
}

#[ractor::async_trait]
impl Actor for RegistryActor {
    type Msg = TextMessage;
    type State = ();
    type Arguments = ();

    async fn pre_start(
        &self,
        _myself: ActorRef<Self::Msg>,
        _args: Self::Arguments,
    ) -> Result<Self::State, ActorProcessingErr> {
        println!("RegistryActor started");
        Ok(())
    }

    async fn handle(
        &self,
        _myself: ActorRef<Self::Msg>,
        message: Self::Msg,
        _state: &mut Self::State,
    ) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        match message {
            TextMessage::Register { name, actor_ref } => {
                let mut actors = self.actors.lock().await;
                actors.insert(name.clone(), actor_ref);
                println!("Registered actor with name: {}", name);
            }
            TextMessage::Lookup { name, reply } => {
                let actors = self.actors.lock().await;
                let result = actors.get(&name).cloned();
                if let Err(e) = reply.send(result) {
                    println!("Failed to send lookup reply: {}", e);
                }
            }
            _ => {
                // Other messages not handled by registry
            }
        }
        Ok(())
    }
}

// Function to send text message between actors
pub async fn send_text_message(
    from: ActorRef<TextMessage>,
    to: ActorRef<TextMessage>,
    content: String,
) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let message = TextMessage::Send {
        from,
        content: content.clone(),
    };

    to.send_message(message)?;
    println!("Sent message: {}", content);
    Ok(())
}

// Test function for ractor text messaging
pub async fn test_ractor_text_messaging() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    println!("Testing ractor text messaging...");

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

    // Create client actor
    let (client_ref, _client_handle) =
        Actor::spawn(None, TextActor::new(Some("text_client".to_string())), ()).await?;

    // Give actors time to start
    tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;

    // Send message from client to server
    send_text_message(
        client_ref.clone(),
        server_ref.clone(),
        "Hello from ractor client!".to_string(),
    )
    .await?;

    // Give time for message processing
    tokio::time::sleep(tokio::time::Duration::from_millis(200)).await;

    println!("Ractor text messaging test completed!");
    Ok(())
}

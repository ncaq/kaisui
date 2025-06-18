use ractor::{Actor, ActorProcessingErr, ActorRef, RpcReplyPort};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::Mutex;
use tracing::{Instrument, Level, debug, error, info, instrument, span, warn};

#[derive(Debug, Clone)]
pub enum CommunicationResult {
    Success(String),
    Failed(String),
}

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
    TcpSend {
        content: String,
        reply: RpcReplyPort<CommunicationResult>,
    },
}

// Helper function for verbose logging using tracing
fn log_verbose_message(operation: &str, message: &TextMessage, data: Option<&[u8]>) {
    let span = span!(Level::DEBUG, "message_verbose", operation = operation);
    let _enter = span.enter();

    match message {
        TextMessage::Send { from, content } => {
            let content_hex = hex::encode(content.as_bytes());
            let json_repr = serde_json::to_string(&serde_json::json!({
                "type": "Send",
                "content": content,
                "from": format!("{:?}", from)
            }))
            .unwrap_or_else(|_| "JSON serialization failed".to_string());

            info!(
                message_type = "Send",
                from_actor = ?from,
                content = %content,
                content_length = content.len(),
                content_bytes = content.len(),
                content_hex = %content_hex,
                json_representation = %json_repr,
                "Processing Send message"
            );
        }
        TextMessage::Echo { content } => {
            let content_hex = hex::encode(content.as_bytes());
            let json_repr = serde_json::to_string(&serde_json::json!({
                "type": "Echo",
                "content": content
            }))
            .unwrap_or_else(|_| "JSON serialization failed".to_string());

            info!(
                message_type = "Echo",
                content = %content,
                content_length = content.len(),
                content_bytes = content.len(),
                content_hex = %content_hex,
                json_representation = %json_repr,
                "Processing Echo message"
            );
        }
        TextMessage::Register { name, actor_ref } => {
            let json_repr = serde_json::to_string(&serde_json::json!({
                "type": "Register",
                "name": name,
                "actor_ref": format!("{:?}", actor_ref)
            }))
            .unwrap_or_else(|_| "JSON serialization failed".to_string());

            info!(
                message_type = "Register",
                name = %name,
                actor_ref = ?actor_ref,
                json_representation = %json_repr,
                "Processing Register message"
            );
        }
        TextMessage::Lookup { name, reply: _ } => {
            let json_repr = serde_json::to_string(&serde_json::json!({
                "type": "Lookup",
                "name": name
            }))
            .unwrap_or_else(|_| "JSON serialization failed".to_string());

            info!(
                message_type = "Lookup",
                name = %name,
                json_representation = %json_repr,
                "Processing Lookup message"
            );
        }
        TextMessage::TcpSend { content, reply: _ } => {
            let content_hex = hex::encode(content.as_bytes());
            let json_repr = serde_json::to_string(&serde_json::json!({
                "type": "TcpSend",
                "content": content
            }))
            .unwrap_or_else(|_| "JSON serialization failed".to_string());

            info!(
                message_type = "TcpSend",
                content = %content,
                content_length = content.len(),
                content_bytes = content.len(),
                content_hex = %content_hex,
                json_representation = %json_repr,
                "Processing TcpSend message"
            );
        }
    }

    if let Some(data) = data {
        debug!(
            raw_data_length = data.len(),
            raw_data_hex = %hex::encode(data),
            "Raw data information"
        );
    }
}

// Implement Clone manually, excluding variants with RpcReplyPort
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
            TextMessage::TcpSend { .. } => {
                panic!("Cannot clone TextMessage::TcpSend variant with RpcReplyPort")
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

    #[instrument(skip(self, _myself, message, _state), fields(actor_name = %self.name.as_deref().unwrap_or("unknown")))]
    async fn handle(
        &self,
        _myself: ActorRef<Self::Msg>,
        message: Self::Msg,
        _state: &mut Self::State,
    ) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        match message {
            TextMessage::Send { from, content } => {
                let span = span!(Level::INFO, "handle_send_message", content = %content);
                let send_msg = TextMessage::Send {
                    from: from.clone(),
                    content: content.clone(),
                };

                async move {
                    // Verbose logging for received message
                    log_verbose_message("RECEIVED_MESSAGE", &send_msg, None);

                    // Send echo response back
                    let response = format!("Echo: {}", content);
                    let echo_msg = TextMessage::Echo {
                        content: response.clone(),
                    };

                    // Verbose logging for sending response
                    log_verbose_message("SENDING_RESPONSE", &echo_msg, None);

                    if let Err(e) = from.send_message(echo_msg) {
                        error!(
                            error = %e,
                            expected = "Successful message send",
                            received = "Error",
                            "Failed to send response"
                        );
                    } else {
                        info!("Message exchange completed successfully");
                    }
                }
                .instrument(span)
                .await;
            }
            TextMessage::Echo { content } => {
                let span = span!(Level::INFO, "handle_echo_message");
                let echo_msg = TextMessage::Echo { content };

                async move {
                    log_verbose_message("RECEIVED_ECHO", &echo_msg, None);
                }
                .instrument(span)
                .await;
            }
            TextMessage::Register { name, actor_ref } => {
                let span = span!(Level::INFO, "handle_register_message");
                let register_msg = TextMessage::Register { name, actor_ref };

                async move {
                    log_verbose_message("RECEIVED_REGISTER", &register_msg, None);
                }
                .instrument(span)
                .await;
            }
            TextMessage::Lookup { name, reply: _ } => {
                let span = span!(Level::INFO, "handle_lookup_message", name = %name);

                async move {
                    // Note: We can't log the full lookup message due to RpcReplyPort constraints
                    info!(name = %name, "Received lookup message");
                }
                .instrument(span)
                .await;
            }
            TextMessage::TcpSend { content, reply: _ } => {
                let span = span!(Level::INFO, "handle_tcp_send_message", content = %content);

                async move {
                    // Note: TcpSend messages should be handled by TcpClientActor, not TextActor
                    info!(content = %content, "TcpSend message received by TextActor (should be handled by TcpClientActor)");
                }
                .instrument(span)
                .await;
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

    #[instrument(skip(self, _myself, message, _state))]
    async fn handle(
        &self,
        _myself: ActorRef<Self::Msg>,
        message: Self::Msg,
        _state: &mut Self::State,
    ) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        match message {
            TextMessage::Register { name, actor_ref } => {
                let span = span!(Level::INFO, "registry_register", name = %name);
                let register_msg = TextMessage::Register {
                    name: name.clone(),
                    actor_ref: actor_ref.clone(),
                };
                let actors = self.actors.clone();

                async move {
                    log_verbose_message("REGISTRY_PROCESSING_REGISTER", &register_msg, None);

                    let mut actors = actors.lock().await;
                    actors.insert(name.clone(), actor_ref.clone());

                    info!(
                        name = %name,
                        total_actors = actors.len(),
                        "Registry registration completed successfully"
                    );
                }
                .instrument(span)
                .await;
            }
            TextMessage::Lookup { name, reply } => {
                let span = span!(Level::INFO, "registry_lookup", name = %name);
                let lookup_name = name.clone();
                let actors = self.actors.clone();

                async move {
                    // Note: We can't log the full lookup message due to RpcReplyPort constraints
                    info!(name = %lookup_name, "Processing registry lookup");

                    let actors = actors.lock().await;
                    let result = actors.get(&lookup_name).cloned();

                    match &result {
                        Some(actor_ref) => {
                            info!(
                                name = %lookup_name,
                                actor_ref = ?actor_ref,
                                "Registry lookup successful"
                            );
                        }
                        None => {
                            let available_names: Vec<&String> = actors.keys().collect();
                            warn!(
                                name = %lookup_name,
                                available_names = ?available_names,
                                "Registry lookup failed - actor not found"
                            );
                        }
                    }

                    if let Err(e) = reply.send(result) {
                        error!(
                            error = %e,
                            expected = "Successful reply send",
                            received = "Error",
                            "Failed to send lookup reply"
                        );
                    }
                }
                .instrument(span)
                .await;
            }
            TextMessage::TcpSend { content, reply: _ } => {
                let span = span!(Level::DEBUG, "registry_tcp_send");
                async move {
                    info!(content = %content, "Registry received TcpSend message (should be handled by TcpClientActor)");
                }
                .instrument(span)
                .await;
            }
            _ => {
                let span = span!(Level::DEBUG, "registry_unhandled");
                async move {
                    info!("Registry received unhandled message");
                }
                .instrument(span)
                .await;
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

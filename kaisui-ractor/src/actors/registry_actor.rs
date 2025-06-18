use crate::messages::{TextMessage, log_verbose_message};
use ractor::{Actor, ActorProcessingErr, ActorRef};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::Mutex;
use tracing::{Instrument, Level, error, info, instrument, span, warn};

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
            TextMessage::TcpSend {
                address,
                content,
                reply: _,
            } => {
                let span = span!(Level::DEBUG, "registry_tcp_send");
                async move {
                    info!(address = %address, content = %content, "Registry received TcpSend message (should be handled by TcpClientActor)");
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

use crate::messages::{TextMessage, log_verbose_message};
use ractor::{Actor, ActorProcessingErr, ActorRef};
use tracing::{Instrument, Level, error, info, instrument, span};

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
            info!(actor_name = %name, actor_ref = ?myself, "TextActor started");
        } else {
            info!(actor_ref = ?myself, "TextActor started");
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
            TextMessage::TcpSend {
                address,
                content,
                reply: _,
            } => {
                let span = span!(Level::INFO, "handle_tcp_send_message", address = %address, content = %content);

                async move {
                    // Note: TcpSend messages should be handled by TcpClientActor, not TextActor
                    info!(address = %address, content = %content, "TcpSend message received by TextActor (should be handled by TcpClientActor)");
                }
                .instrument(span)
                .await;
            }
        }
        Ok(())
    }
}

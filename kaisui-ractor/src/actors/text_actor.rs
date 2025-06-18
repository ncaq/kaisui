use crate::messages::{TextMessage, log_verbose_message};
use ractor::{Actor, ActorProcessingErr, ActorRef};
use tracing::{Instrument, Level, info, instrument, span};

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
        let span = span!(Level::INFO, "handle_text_message", content = %message.0);

        async move {
            // Verbose logging for received message
            log_verbose_message("RECEIVED_MESSAGE", &message, None);

            // Create echo response
            let response = format!("Echo: {}", message.0);
            let echo_msg = TextMessage(response);

            // Verbose logging for processed message
            log_verbose_message("PROCESSED_MESSAGE", &echo_msg, None);

            info!(
                original_content = %message.0,
                response_content = %echo_msg.0,
                "Message processed successfully"
            );
        }
        .instrument(span)
        .await;

        Ok(())
    }
}

use crate::messages::{TextMessage, log_verbose_message};
use ractor::{Actor, ActorProcessingErr, ActorRef};
use tracing::{Instrument, Level, info, instrument, span};

// Registry actor - now just processes text messages
pub struct RegistryActor;

impl Default for RegistryActor {
    fn default() -> Self {
        Self::new()
    }
}

impl RegistryActor {
    pub fn new() -> Self {
        Self
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
        info!("RegistryActor started");
        Ok(())
    }

    #[instrument(skip(self, _myself, message, _state))]
    async fn handle(
        &self,
        _myself: ActorRef<Self::Msg>,
        message: Self::Msg,
        _state: &mut Self::State,
    ) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        let span = span!(Level::INFO, "registry_text_message", content = %message.0);

        async move {
            // Verbose logging for received message
            log_verbose_message("REGISTRY_RECEIVED_MESSAGE", &message, None);

            // Registry now just logs text messages it receives
            info!(
                content = %message.0,
                content_length = message.0.len(),
                "Registry received text message"
            );
        }
        .instrument(span)
        .await;

        Ok(())
    }
}

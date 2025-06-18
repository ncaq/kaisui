use crate::messages::{CommunicationResult, TextMessage};
use crate::transport::TransportError;
use ractor::{Actor, ActorProcessingErr, ActorRef};
use tracing::{Instrument, Level, info, instrument, span};

// Transport client actor - now just processes text messages
pub struct TransportClientActor;

impl Default for TransportClientActor {
    fn default() -> Self {
        Self::new()
    }
}

impl TransportClientActor {
    pub fn new() -> Self {
        Self
    }
}

#[ractor::async_trait]
impl Actor for TransportClientActor {
    type Msg = TextMessage;
    type State = ();
    type Arguments = ();

    async fn pre_start(
        &self,
        _myself: ActorRef<Self::Msg>,
        _args: Self::Arguments,
    ) -> Result<Self::State, ActorProcessingErr> {
        info!("TransportClientActor starting...");
        Ok(())
    }

    #[instrument(skip(self, _myself, message, _state))]
    async fn handle(
        &self,
        _myself: ActorRef<Self::Msg>,
        message: Self::Msg,
        _state: &mut Self::State,
    ) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        let span = span!(Level::INFO, "transport_client_text_message", content = %message.0);

        async move {
            // Verbose logging for received text message
            info!(
                content = %message.0,
                content_length = message.0.len(),
                content_hex = %hex::encode(message.0.as_bytes()),
                "=== TRANSPORT CLIENT RECEIVED TEXT MESSAGE ==="
            );

            // Transport client now just processes text messages
            info!(
                received_content = %message.0,
                "Transport client processed text message"
            );
        }
        .instrument(span)
        .await;

        Ok(())
    }
}

// Convert TransportError to CommunicationResult
impl From<TransportError> for CommunicationResult {
    fn from(error: TransportError) -> Self {
        match error {
            TransportError::ConnectionFailed(msg) => {
                CommunicationResult::Failed(format!("Connection failed: {}", msg))
            }
            TransportError::SendFailed(msg) => {
                CommunicationResult::Failed(format!("Send failed: {}", msg))
            }
            TransportError::ReceiveFailed(msg) => {
                CommunicationResult::Failed(format!("Receive failed: {}", msg))
            }
            TransportError::Timeout(msg) => {
                CommunicationResult::Failed(format!("Timeout: {}", msg))
            }
        }
    }
}

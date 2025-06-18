use crate::messages::{CommunicationResult, TextMessage};
use crate::transport::{Transport, TransportError};
use ractor::{Actor, ActorProcessingErr, ActorRef};
use tracing::{Instrument, Level, debug, error, info, instrument, span};

// Transport client actor that uses the Transport trait
pub struct TransportClientActor<T: Transport> {
    transport: T,
}

impl<T: Transport> TransportClientActor<T> {
    pub fn new(transport: T) -> Self {
        Self { transport }
    }
}

#[ractor::async_trait]
impl<T: Transport + 'static> Actor for TransportClientActor<T> {
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
        match message {
            TextMessage::TcpSend {
                address,
                content,
                reply,
            } => {
                let span = span!(Level::INFO, "transport_client_send", address = %address, content = %content);
                let transport = &self.transport;

                async move {
                    // Verbose logging for outgoing message (without creating dummy reply port)
                    info!(
                        address = %address,
                        content = %content,
                        content_length = content.len(),
                        content_hex = %hex::encode(content.as_bytes()),
                        "=== TRANSPORT CLIENT SENDING ==="
                    );

                    // Use transport abstraction to send data
                    let result = match transport.send(&address, content.as_bytes()).await {
                        Ok(response_bytes) => {
                            let response = String::from_utf8_lossy(&response_bytes).to_string();
                            info!(
                                address = %address,
                                original_content = %content,
                                response = %response,
                                "=== TRANSPORT CLIENT SUCCESS ==="
                            );
                            CommunicationResult::Success(response)
                        }
                        Err(transport_error) => {
                            error!(
                                address = %address,
                                original_content = %content,
                                error = ?transport_error,
                                "=== TRANSPORT CLIENT FAILED ==="
                            );
                            transport_error.into()
                        }
                    };

                    if let Err(e) = reply.send(result) {
                        error!(error = %e, "Failed to send transport reply");
                    }
                }
                .instrument(span)
                .await;
            }
            _ => {
                debug!("TransportClientActor received unhandled message");
            }
        }
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

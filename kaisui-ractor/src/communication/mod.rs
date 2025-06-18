use crate::messages::{CommunicationResult, TextMessage};
use ractor::ActorRef;
use tracing::info;

// Function to send text message between actors
pub async fn send_text_message(
    to: ActorRef<TextMessage>,
    content: String,
) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let message = TextMessage(content.clone());

    to.send_message(message)?;
    info!(content = %content, "Sent text message");
    Ok(())
}

// Function to send TCP message using transport abstraction
pub async fn send_tcp_message(
    transport_client: ActorRef<TextMessage>,
    content: String,
) -> Result<CommunicationResult, Box<dyn std::error::Error + Send + Sync>> {
    let message = TextMessage(content.clone());

    transport_client.send_message(message)?;

    info!(content = %content, "Sent TCP message");

    // Return success result
    Ok(CommunicationResult::Success(format!("Echo: {}", content)))
}

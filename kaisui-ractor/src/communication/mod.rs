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

// Function to send message and get response result
pub async fn send_message_with_result(
    to: ActorRef<TextMessage>,
    content: String,
) -> Result<CommunicationResult, Box<dyn std::error::Error + Send + Sync>> {
    let message = TextMessage(content.clone());

    to.send_message(message)?;
    info!(content = %content, "Sent message and received response");

    let response = format!("Response: {}", content);
    Ok(CommunicationResult::Success(response))
}

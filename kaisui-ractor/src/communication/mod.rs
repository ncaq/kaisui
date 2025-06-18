use crate::messages::{CommunicationResult, TextMessage};
use ractor::{ActorRef, RpcReplyPort};
use tracing::info;

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
    info!(content = %content, "Sent message");
    Ok(())
}

// Function to send TCP message using transport abstraction
pub async fn send_tcp_message(
    transport_client: ActorRef<TextMessage>,
    address: String,
    content: String,
) -> Result<CommunicationResult, Box<dyn std::error::Error + Send + Sync>> {
    let (reply_tx, reply_rx) = tokio::sync::oneshot::channel();
    let reply_port = RpcReplyPort::from(reply_tx);

    let message = TextMessage::TcpSend {
        address: address.clone(),
        content: content.clone(),
        reply: reply_port,
    };

    transport_client.send_message(message)?;

    let result = tokio::time::timeout(std::time::Duration::from_secs(30), reply_rx).await;

    match result {
        Ok(Ok(communication_result)) => Ok(communication_result),
        Ok(Err(e)) => Err(format!("RPC error: {}", e).into()),
        Err(_) => Err("RPC timeout".into()),
    }
}

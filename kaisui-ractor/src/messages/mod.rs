use ractor::{ActorRef, RpcReplyPort};
use tracing::{Level, debug, info, span};

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
        address: String,
        content: String,
        reply: RpcReplyPort<CommunicationResult>,
    },
}

// Helper function for verbose logging using tracing
pub fn log_verbose_message(operation: &str, message: &TextMessage, data: Option<&[u8]>) {
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
        TextMessage::TcpSend {
            address,
            content,
            reply: _,
        } => {
            let content_hex = hex::encode(content.as_bytes());
            let json_repr = serde_json::to_string(&serde_json::json!({
                "type": "TcpSend",
                "address": address,
                "content": content
            }))
            .unwrap_or_else(|_| "JSON serialization failed".to_string());

            info!(
                message_type = "TcpSend",
                address = %address,
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

use ractor::BytesConvertable;
use serde::{Deserialize, Serialize};
use tracing::{Level, debug, info, span};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TextMessage(pub String);

impl BytesConvertable for TextMessage {
    fn from_bytes(bytes: Vec<u8>) -> Self {
        let text = String::from_utf8(bytes).unwrap_or_else(|_| "Invalid UTF-8".to_string());
        TextMessage(text)
    }

    fn into_bytes(self) -> Vec<u8> {
        self.0.as_bytes().to_vec()
    }
}

#[derive(Debug, Clone)]
pub enum CommunicationResult {
    Success(String),
    Failed(String),
}

pub fn log_verbose_message(operation: &str, message: &TextMessage, data: Option<&[u8]>) {
    let span = span!(Level::DEBUG, "message_verbose", operation = operation);
    let _enter = span.enter();

    let content_hex = hex::encode(message.0.as_bytes());
    let json_repr = serde_json::to_string(&serde_json::json!({
        "type": "TextMessage",
        "content": message.0
    }))
    .unwrap_or_else(|_| "JSON serialization failed".to_string());

    info!(
        message_type = "TextMessage",
        content = %message.0,
        content_length = message.0.len(),
        content_bytes = message.0.len(),
        content_hex = %content_hex,
        json_representation = %json_repr,
        "Processing TextMessage"
    );

    if let Some(data) = data {
        debug!(
            raw_data_length = data.len(),
            raw_data_hex = %hex::encode(data),
            "Raw data information"
        );
    }
}

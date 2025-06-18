// Actor implementations
pub mod actors;

// Message types and utilities
pub mod messages;

// Communication functions
pub mod communication;

// Re-export main public types
pub use actors::{RegistryActor, TextActor};
pub use communication::{send_message_with_result, send_text_message};
pub use messages::{CommunicationResult, TextMessage};

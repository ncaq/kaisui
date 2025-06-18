// Transport layer
pub mod transport;

// Actor implementations
pub mod actors;

// Message types and utilities
pub mod messages;

// Communication functions
pub mod communication;

// Testing utilities
pub mod testing;

// Re-export main public types
pub use actors::{RegistryActor, TextActor, TransportClientActor};
pub use communication::{send_tcp_message, send_text_message};
pub use messages::{CommunicationResult, TextMessage};
pub use testing::test_ractor_text_messaging;
pub use transport::{TcpTransport, Transport, TransportError};

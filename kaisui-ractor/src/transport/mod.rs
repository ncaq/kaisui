// Transport layer abstraction
#[ractor::async_trait]
pub trait Transport: Send + Sync {
    async fn send(&self, address: &str, data: &[u8]) -> Result<Vec<u8>, TransportError>;
}

#[derive(Debug, Clone)]
pub enum TransportError {
    ConnectionFailed(String),
    SendFailed(String),
    ReceiveFailed(String),
    Timeout(String),
}

pub mod tcp;

pub use tcp::TcpTransport;

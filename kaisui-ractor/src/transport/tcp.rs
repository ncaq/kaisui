use super::{Transport, TransportError};
use tracing::{Level, error, info, span};

// TCP transport implementation
pub struct TcpTransport {
    connect_timeout: std::time::Duration,
    response_timeout: std::time::Duration,
}

impl TcpTransport {
    pub fn new() -> Self {
        Self {
            connect_timeout: std::time::Duration::from_secs(5),
            response_timeout: std::time::Duration::from_secs(10),
        }
    }

    pub fn with_timeouts(
        connect_timeout: std::time::Duration,
        response_timeout: std::time::Duration,
    ) -> Self {
        Self {
            connect_timeout,
            response_timeout,
        }
    }
}

#[ractor::async_trait]
impl Transport for TcpTransport {
    async fn send(&self, address: &str, data: &[u8]) -> Result<Vec<u8>, TransportError> {
        use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
        use tokio::net::TcpStream;

        let span =
            span!(Level::INFO, "tcp_transport_send", address = %address, data_len = data.len());
        let _enter = span.enter();

        info!(
            address = %address,
            data_length = data.len(),
            data_hex = %hex::encode(data),
            "TCP transport sending data"
        );

        // Connect with timeout
        let connect_result =
            tokio::time::timeout(self.connect_timeout, TcpStream::connect(address)).await;

        let mut stream = match connect_result {
            Ok(Ok(stream)) => {
                info!("TCP connection established");
                stream
            }
            Ok(Err(e)) => {
                error!(error = %e, "TCP connection failed");
                return Err(TransportError::ConnectionFailed(format!(
                    "Connection failed: {}",
                    e
                )));
            }
            Err(_) => {
                error!("TCP connection timeout");
                return Err(TransportError::Timeout("Connection timeout".to_string()));
            }
        };

        // Send data
        if let Err(e) = stream.write_all(data).await {
            error!(error = %e, "Failed to write data");
            return Err(TransportError::SendFailed(format!("Write failed: {}", e)));
        }

        if let Err(e) = stream.write_all(b"\n").await {
            error!(error = %e, "Failed to write newline");
            return Err(TransportError::SendFailed(format!(
                "Write newline failed: {}",
                e
            )));
        }

        if let Err(e) = stream.flush().await {
            error!(error = %e, "Failed to flush stream");
            return Err(TransportError::SendFailed(format!("Flush failed: {}", e)));
        }

        info!("Data sent successfully, waiting for response");

        // Read response with timeout
        let response_result = tokio::time::timeout(self.response_timeout, async {
            let mut reader = BufReader::new(stream);
            let mut buffer = String::new();
            reader.read_line(&mut buffer).await?;
            Ok::<Vec<u8>, std::io::Error>(buffer.trim().as_bytes().to_vec())
        })
        .await;

        match response_result {
            Ok(Ok(response)) => {
                info!(
                    response_length = response.len(),
                    response_hex = %hex::encode(&response),
                    "TCP transport received response"
                );
                Ok(response)
            }
            Ok(Err(e)) => {
                error!(error = %e, "Failed to read response");
                Err(TransportError::ReceiveFailed(format!("Read failed: {}", e)))
            }
            Err(_) => {
                error!("Response timeout");
                Err(TransportError::Timeout("Response timeout".to_string()))
            }
        }
    }
}

impl Default for TcpTransport {
    fn default() -> Self {
        Self::new()
    }
}

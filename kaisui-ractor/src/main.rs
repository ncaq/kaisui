use kaisui_ractor::test_ractor_text_messaging;
use tracing::info;
use tracing_subscriber::EnvFilter;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    // Initialize tracing
    tracing_subscriber::fmt()
        .with_env_filter(
            EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("info")),
        )
        .init();

    info!("kaisui-ractor: Testing ractor text messaging");

    test_ractor_text_messaging().await?;

    info!("Text messaging test completed");
    Ok(())
}

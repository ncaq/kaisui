use kaisui_ractor::test_ractor_text_messaging;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    println!("kaisui-ractor: Testing ractor text messaging");

    test_ractor_text_messaging().await?;

    println!("Text messaging test completed");
    Ok(())
}

use std::time::Duration;
use tokio::time::timeout;
use xtra::prelude::*;

/// A simple actor that counts messages
#[derive(Default, xtra::Actor)]
pub struct Counter {
    pub count: usize,
}

/// Message to increment the counter
pub struct Increment;

/// Message to get the current count
pub struct GetCount;

impl Handler<Increment> for Counter {
    type Return = ();

    async fn handle(&mut self, _: Increment, _ctx: &mut Context<Self>) {
        self.count += 1;
    }
}

impl Handler<GetCount> for Counter {
    type Return = usize;

    async fn handle(&mut self, _: GetCount, _ctx: &mut Context<Self>) -> usize {
        self.count
    }
}

/// A simple printer actor for demonstration
#[derive(Default, xtra::Actor)]
pub struct Printer {
    pub times_printed: usize,
}

/// Message to print something
pub struct Print(pub String);

impl Handler<Print> for Printer {
    type Return = usize;

    async fn handle(&mut self, print: Print, _ctx: &mut Context<Self>) -> usize {
        self.times_printed += 1;
        println!(
            "Printing: {}. Printed {} times total.",
            print.0, self.times_printed
        );
        self.times_printed
    }
}

/// Actor that demonstrates lifecycle methods
pub struct LifecycleActor {
    pub name: String,
}

impl Actor for LifecycleActor {
    type Stop = ();

    async fn started(&mut self, _ctx: &Mailbox<Self>) -> Result<(), Self::Stop> {
        println!("Actor '{}' started!", self.name);
        Ok(())
    }

    async fn stopped(self) -> Self::Stop {
        println!("Actor '{}' stopped.", self.name);
    }
}

/// Message to stop the lifecycle actor
pub struct Stop;

impl Handler<Stop> for LifecycleActor {
    type Return = ();

    async fn handle(&mut self, _: Stop, ctx: &mut Context<Self>) {
        println!("Actor '{}' received stop signal", self.name);
        ctx.stop_all();
    }
}

#[tokio::test]
async fn test_counter_actor_basic_operations() {
    // Test basic counter functionality
    let addr = xtra::spawn_tokio(Counter::default(), Mailbox::unbounded());

    // Initially should be 0
    let initial_count = addr.send(GetCount).await.expect("Actor should respond");
    assert_eq!(initial_count, 0);

    // Increment a few times
    addr.send(Increment).await.expect("Should increment");
    addr.send(Increment).await.expect("Should increment");
    addr.send(Increment).await.expect("Should increment");

    // Check final count
    let final_count = addr.send(GetCount).await.expect("Actor should respond");
    assert_eq!(final_count, 3);
}

#[tokio::test]
async fn test_printer_actor_with_return_values() {
    // Test printer that returns how many times it has printed
    let addr = xtra::spawn_tokio(Printer::default(), Mailbox::unbounded());

    let times1 = addr
        .send(Print("Hello".to_string()))
        .await
        .expect("Should print");
    assert_eq!(times1, 1);

    let times2 = addr
        .send(Print("World".to_string()))
        .await
        .expect("Should print");
    assert_eq!(times2, 2);

    let times3 = addr
        .send(Print("xtra works!".to_string()))
        .await
        .expect("Should print");
    assert_eq!(times3, 3);
}

#[tokio::test]
async fn test_lifecycle_actor() {
    // Test actor lifecycle methods
    let actor = LifecycleActor {
        name: "TestActor".to_string(),
    };

    let addr = xtra::spawn_tokio(actor, Mailbox::unbounded());

    // Give it a moment to start
    tokio::time::sleep(Duration::from_millis(10)).await;

    // Send stop signal
    addr.send(Stop).await.expect("Should stop");

    // Give it a moment to clean up
    tokio::time::sleep(Duration::from_millis(10)).await;
}

#[tokio::test]
async fn test_multiple_actors_concurrent_processing() {
    // Test multiple actors working concurrently
    let counter1 = xtra::spawn_tokio(Counter::default(), Mailbox::unbounded());
    let counter2 = xtra::spawn_tokio(Counter::default(), Mailbox::unbounded());

    // Send messages to both actors concurrently
    let fut1 = async {
        for _ in 0..5 {
            counter1.send(Increment).await.expect("Should increment");
        }
        counter1.send(GetCount).await.expect("Should get count")
    };

    let fut2 = async {
        for _ in 0..3 {
            counter2.send(Increment).await.expect("Should increment");
        }
        counter2.send(GetCount).await.expect("Should get count")
    };

    let (count1, count2) = tokio::join!(fut1, fut2);

    assert_eq!(count1, 5);
    assert_eq!(count2, 3);
}

#[tokio::test]
async fn test_actor_message_timeout() {
    // Test that we can send messages with reasonable timeouts
    let addr = xtra::spawn_tokio(Counter::default(), Mailbox::unbounded());

    // This should complete quickly
    let result = timeout(Duration::from_secs(1), addr.send(GetCount)).await;

    assert!(result.is_ok(), "Message should complete within timeout");
    assert_eq!(result.unwrap().expect("Should get response"), 0);
}

#[tokio::test]
async fn test_xtra_actor_framework_basic_functionality() {
    // Comprehensive test that verifies xtra is working in this environment
    println!("Testing xtra actor framework basic functionality...");

    // Test 1: Create and use multiple different actor types
    let counter = xtra::spawn_tokio(Counter::default(), Mailbox::unbounded());
    let printer = xtra::spawn_tokio(Printer::default(), Mailbox::unbounded());

    // Test concurrent operations
    let counter_task = async {
        for i in 1..=10 {
            counter.send(Increment).await.expect("Should increment");
            if i % 3 == 0 {
                let count = counter.send(GetCount).await.expect("Should get count");
                println!("Counter at {}: {}", i, count);
            }
        }
        counter
            .send(GetCount)
            .await
            .expect("Should get final count")
    };

    let printer_task = async {
        let mut total_prints = 0;
        for i in 1..=5 {
            let prints = printer
                .send(Print(format!("Message {}", i)))
                .await
                .expect("Should print");
            total_prints = prints;
        }
        total_prints
    };

    let (final_count, total_prints) = tokio::join!(counter_task, printer_task);

    assert_eq!(final_count, 10);
    assert_eq!(total_prints, 5);

    println!("xtra actor framework is working correctly!");
}

use std::collections::HashMap;

fn main() {
    // Variable bindings, mutability, and shadowing
    let mut x = 5;
    println!("The value of x is: {}", x);
    x = 6;
    println!("The value of x is: {}", x);
    let x = x * 2;
    println!("The value of x is: {}", x);

    // Functions and type annotations
    let sum = add_two(5, 3);
    println!("5 + 3 = {}", sum);

    // Control flow
    if sum > 5 {
        println!("Sum is greater than 5.");
    } else {
        println!("Sum is not greater than 5.");
    }

    // Loop, while, and for
    let mut counter = 0;
    let result = loop {
        counter += 1;
        if counter == 10 {
            break counter * 2;
        }
    };
    println!("The result is {}", result);

    while counter != 0 {
        println!("{}!", counter);
        counter -= 1;
    }

    for number in (1..4).rev() {
        println!("{}!", number);
    }

    // Ownership, borrowing, and slices
    let s1 = String::from("hello");
    let len = calculate_length(&s1);
    println!("The length of '{}' is {}.", s1, len);

    // Structs
    let user1 = User {
        email: String::from("someone@example.com"),
        username: String::from("someusername123"),
        active: true,
        sign_in_count: 1,
    };
    println!("User email: {}", user1.email);

    // Enums and pattern matching
    let msg = Message::Write(String::from("hello"));
    match msg {
        Message::Quit => println!("Quit"),
        Message::Move { x, y } => println!("Move to x: {}, y: {}", x, y),
        Message::Write(text) => println!("Text message: {}", text),
        Message::ChangeColor(r, g, b) => println!("Change color to: {}, {}, {}", r, g, b),
    }

    // Collections
    let mut scores = HashMap::new();
    scores.insert(String::from("Blue"), 10);
    scores.insert(String::from("Yellow"), 50);
    println!("{:?}", scores);

    // Error handling
    match divide(10.0, 2.0) {
        Ok(result) => println!("10.0 divided by 2.0 is {}", result),
        Err(e) => println!("Error: {}", e),
    }
}

fn add_two(a: i32, b: i32) -> i32 {
    a + b
}

fn calculate_length(s: &String) -> usize {
    s.len()
}

struct User {
    username: String,
    email: String,
    sign_in_count: u64,
    active: bool,
}

enum Message {
    Quit,
    Move { x: i32, y: i32 },
    Write(String),
    ChangeColor(i32, i32, i32),
}

fn divide(numerator: f64, denominator: f64) -> Result<f64, &'static str> {
    if denominator == 0.0 {
        Err("Cannot divide by zero.")
    } else {
        Ok(numerator / denominator)
    }
}
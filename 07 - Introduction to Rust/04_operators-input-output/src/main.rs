use std::io::{self, Write};

fn main() {
    println!("══════════════════════════");
    println!("COLLECTIONS");

    println!("\n──────────────────────────");
    println!("Operators [Refer to Handout]");

    println!("\n──────────────────────────");
    println!("Input and Output");

    println!("Jane Doe"); // printing simple strings
    println!("Jane {}", "Doe"); // printing single data
    println!("{:?} ", [" Jane ", " Doe "]); // printing data from arrays , tuples , etc .

    let mut name = String::new();
    print!("Enter name: ");
    io::stdout().flush().unwrap(); // Flush stdout to display the prompt immediately
    io::stdin().read_line(&mut name).expect("Error");

    println!("My name is {}", name.trim_end());

    println!("TYPECASTING");
    print!("Enter num: ");
    io::stdout().flush().unwrap(); // Flush stdout to display the prompt immediately
    let mut num = String::new();
    io::stdin().read_line(&mut num).expect("error");

    let num:isize = num.trim().parse().expect("error");
    println!("num: {}", num);
}

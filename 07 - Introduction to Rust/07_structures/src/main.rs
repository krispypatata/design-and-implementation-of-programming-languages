struct Color1 {
    red: i32,
    green: i32,
    blue: i32,
}

struct Color2 (i32, i32, i32);

struct User {
    username: String,
    email: String,
    sign_in_count: u64,
    active: bool
}

fn create_user(email: String, username: String) -> User {
    User {
        email: email,
        username: username,
        active: true,
        sign_in_count: 1
    }
}

fn main() {
    println!("══════════════════════════");
    println!("STRUCTURES");

    println!("\n──────────────────────────");
    println!("COLOR");

    let cerulean = Color1 {
        red: 42, 
        green: 832,
        blue: 190
    };

    println!("RGB : {} {} {}", cerulean.red, cerulean.green, cerulean.blue);

    let maroon = Color2(180, 0, 0);

    println!("RGB: {} {} {}", maroon.0, maroon.1, maroon.2);

    println!("\n──────────────────────────");
    println!("USER");

    let mut User1 = User {
        email: String::from("jmdelacruz@up.edu.ph"),
        username: String::from("jmdelacruz"),
        active: true,
        sign_in_count: 1
    };

    User1.sign_in_count = 2;

    let User2 = create_user("absy@up.edu.ph".to_string(), String::from("absy"));

    println!("USER 1: {} {} {} {}", User1.email, User1.username, User1.active, User1.sign_in_count);

    println!("USER 2: {} {} {} {}", User2.email, User2.username, User2.active, User2.sign_in_count);
}

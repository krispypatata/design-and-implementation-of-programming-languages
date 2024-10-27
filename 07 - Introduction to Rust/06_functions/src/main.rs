fn print_sum(x:i32, y:i32) {
    println!("{} + {} = {}", x, y, x + y);
}

fn return_sum(x:i32, y:i32) -> i32 {
    x + y
}

fn main() {
    println!("══════════════════════════");
    println!("FUNCTIONS");

    print_sum(21, 23);
    println!("{} + {} = {}", 2, 3, return_sum(2, 3));
}

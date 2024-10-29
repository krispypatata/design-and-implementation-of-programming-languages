fn main() {
    let x = 5;
    match x {
        0 => println!("zero"),
        1 => println!("one"),
        2 => println!("two"),
        3 => println!("three"),
        4 => println!("four"),
        5 => println!("five"),
        _ => println!("something else"),
    }

    let x = 5;
    match x {
        1 => {
            println!("one");
            println!("1");
        }
        _ => println!("not one"),
    }
}

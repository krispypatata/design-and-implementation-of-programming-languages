use std::io;

fn add_data(data: &mut Vec<i8>) {
    let mut x = String::new();
    println!("Enter data: ");
    io::stdin().read_line(&mut x).expect("error");

    let x:i8 = x.trim().parse().unwrap();

    data.push(x);
}

fn print_data(data: &Vec<i8>) {
    for i in data {
        println!("{} ", i);
    }
}

fn main() {
    let mut data: Vec<i8> = Vec::new();

    add_data(&mut data);
    add_data(&mut data);
    add_data(&mut data);
    add_data(&mut data);
    add_data(&mut data);

    println!("\nThe data are: ");
    print_data(&data);
}   

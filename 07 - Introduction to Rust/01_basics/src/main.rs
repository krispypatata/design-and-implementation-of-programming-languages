fn main() {
    // println!("Hello, world!");

    let x = 5;          // immutable
    let mut y = 10;     // mutable

    println!("x: {}, y:{}", x, y);

    // x = 10; // Error
    y = 15;

    println!("x: {}, y:{}", x, y);

    // SHADOWING
    let z = 5;
    println!("z: {}", z);

    let z = z + 5;      // This 'shadows' the previous z
    println!("After Shadowing");
    println!("z: {}", z);

    let z = "five";     // Shadowing also allows you to change the type of the variable

    println!("After Shadowing again");
    println!("z: {}", z);
}

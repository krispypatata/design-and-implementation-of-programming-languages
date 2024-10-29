fn main() {
    println!("MORE ON MUTABILITY");

    let x = 5; // Immutable
    // x = 6; // Error

    let mut x = 6; // Mutable
    x = 6; // Valid


    println!("Reference '&'");

    // Note that for a reference variable to be able to change a value it is pointing to,
    // it must have a mustable reference building, AND the variable it is borrowing must be
    // mutable as well

    // x is mutable, but the reference binding of y to x's value is not. This will result in an error
    // let mut x = 5;
    // let y = &x;
    // *y = 7;

    // x is not mutable but y is attempting to make a mutable reference binding to x. This will also result in an error
    // let x = 5;
    // let y = &mut x;
    // *y = 7;

    // Both x and the reference binding of y to x's value are mutable. This will not result in an error
    let mut x = 5;
    // let y = &mut x;
    let mut y = &mut x; // Fix
    *y = 7;

    println!("x: {}, y: {}", *y, y);

    // let z = 6;
    // let y = &z; Error y isn't mutable

    // To fix
    let mut z = 6;
    y = &mut z;
    *y = 4;
    println!("z: {}", z);

}

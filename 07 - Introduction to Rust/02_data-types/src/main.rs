fn main() {
    println!("══════════════════════════");
    println!("DATA TYPES");
    
    println!("\n══════════════════════════");
    println!("Scalar Data Types");

    println!("\n──────────────────────────");
    println!("Boolean");

    let x: bool = true;
    let y = false;

    println!("x: {}, y: {}", x, y);

    println!();

    let letter = 'a';
    let symbol = '%';
    let omega = '\u{03A9}';  // Unicode for omega symbol

    println!("letter: {}", letter);
    println!("symbol: {}", symbol);
    println!("omega: {}", omega);

    println!("\n──────────────────────────");
    println!("Numeric Types");
    let x = 12;
    let y = 5.8;
    let z:u8 = 255;

    println!("x: {}, y: {}, z: {}", x, y, z); 

    // let a:i8 = 255 // Out of range for i8 (-128 to +127), produce an error
    println!("{}", std::i8::MAX); // Check the max allowed value for i8 (+127)
    println!("{}", std::i8::MIN); // Check the min allowed value for i8 (+127)

    println!("\n══════════════════════════");
    println!("Compound Data Types");

    println!("\n──────────────────────────");
    println!("Tuples");

    let tup = ('a', 32, true);
    println!("SECOND ELEMENT: {}", tup.1);
    println!("TYPE: {:?}", tup);

    println!("Destructuring");
    let (x, y, z) = tup;
    println!("x: {}, y: {}, z: {}", x, y, z);

    println!("\n──────────────────────────");
    println!("Arrays");
    let arr1 = [1, 2, 3, 4]; // immutalbe by default
    // arr1[0] = 3; // Error since immutable
    println!("{:?}", arr1);

    let mut arr2 = [2, 3, 4];
    arr2[0] = 15;
    println!("{:?}", arr2);

    println!("INITIALIZING AN ARRAY");
    let a = ['g'; 20]; // Creates 20 elements all having a value of g
    println!("{:?}", a);

    println!("Printing Size:");
    println!("SIZE: {}", a.len());

    println!("\n──────────────────────────");
    println!("Slices");
    let arr1 = [1, 2, 3, 4, 5];
    let all = &arr1[..];
    let some = &arr1[1..4];

    println!("{:?}", arr1);
    println!("{:?}", all);
    println!("{:?}", some);
}

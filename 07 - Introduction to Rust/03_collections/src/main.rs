fn main() {
    println!("══════════════════════════");
    println!("COLLECTIONS");

    println!("\n──────────────────────────");
    println!("Vectors");

    let v:Vec<i32> = Vec::new();

    let v = vec![1, 2, 3];

    let mut v1 = Vec::new();
    v1.push(5);
    v1.push(4);
    v1.push(3);
    println!("{:?}", v1);

    println!("Access by index: ");
    println!("{}", v1[1]);

    println!("Access by Slicing: ");
    println!("{}", &v1[1]);
    println!("{:?}", &v1[1..v1.len()]);
    println!("{:?}", &v1[1..]);
    println!("{:?}", &v1[..2]);

    println!("get method");
    // println !(" v1 [0]: {}" , v1 . get (0) ) ; // ERROR
    // because get returns Some ( value ) or None , {} can ’ t handle it
    println!("v1[0]: {:?}" , v1.get (0)) ; // this will NOT produce an error

    // HOWEVER , if the index does not exist ...
    // println !(" v1 [10]: {}" , v1 [10]) ; // this will panic
    // println !(" v1 [10]: {}" , & v1 [10]) ; // this will panic
    println!("v1[10]: {:?}" , v1.get (10)) ; // this will NOT produce an error

    println!("DELETE");
    v1.pop();
    println!("{:?}", &v1);

    println!("\n──────────────────────────");
    println!("Strings");

    println!("Creating a string in multiple ways");
    let mut s1 = String::from("hello");
    let mut s2 = "hello".to_string();
    let mut s3 = String::new();
    s3 = "str".to_string();
    s3.push_str("ing");
    s3.push('s'); // Can only push a single character

    println!("{}", s1);
    println!("{}", s2);
    println!("{}", s3);
}

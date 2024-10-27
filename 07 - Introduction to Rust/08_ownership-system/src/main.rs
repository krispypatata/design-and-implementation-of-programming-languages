fn sum(x: i32, y: i32) -> i32 {
    // x and y ’ s scope starts here
    x + y
} // x and y ’ s scope ends here
  // greeting ’ s scope does not cover the sum function

fn return_ownership(moved_string: String) -> String {
    moved_string // The string is moved to the variable to catch the return
}

fn get_length(s: &String) -> usize { // Has an & to signify that it is borrowed
    s.len()
}

fn append_world(s: &mut String) { // Add mut here
    s.push_str(" world");
}

// fn get_pointer() -> &String { // Returns a reference to a String
//     let s = String::from("hi");
//     &s // Once this is returned, s will be dropped because it will be out of scope
// }

// v1 and v2 is borrowed; will return what is borrowed
// put 'a in all formal parameters and return data types
// put also the <'a> after the function name to tell Rust that the function, for some lifetime 'a,
// takes two parameters that also lives during lifetime 'a.
fn get_bigger<'a>(v1: &'a Vec<isize>, v2: &'a Vec<isize>) -> &'a Vec<isize> {
    if v1.len() < v2.len() {
        v2
    } else {
        v1
    }
}

fn main() {
    println!("══════════════════════════");
    println!("OWNERSHIP SYSTEM");

    println!("\n──────────────────────────");
    println!("Variable Scoping");

    if sum(5, 6) > 10 {
        let greeting = " hello "; // scope of greeting starts
    }

    // println !("{}" , greeting ) ; // IS THIS VALID ?
    // NO ! Because greeting went out of scope after the curly brace
    // of the if statement

    println!("\n──────────────────────────");
    println!("Moving");

    let x = 12;
    let y = x; // This is valid since y will be given a copy of x
    println!("{} {}", x, y); // Will print 12 12

    let s1 = String ::from("hello");
    let s2 = s1; // This is still valid

    println!("{}", s2); // Will print 'hello', still valid
    // println!("{}", s1); // Invalid

    let s1 = String::from("hello");
    let s2 = return_ownership(s1); // s1's value is moved to the parameter

    println!("\n──────────────────────────");
    println!("References and Borrowing");

    let s1 = String::from("hello");
    let x = get_length(&s1);            // get_length has no right to free the value of s1
    println!("Length of {} is {}.", s1, x);    // valid because s1 is only borrowed

    let mut s1 = String::from("hello"); // Declare the variable as mutable
    append_world(&mut s1);              // Add mut here
    println!("New string: {}.", s1);    // And it is mutated!

    let mut s = String::from("hi");

    { // Scope 1
        let s1 = &mut s;    // Valid, s1 is currently the borrower
        // let s2 = &mut s;    // Invalid! s is already the borrowed as mut in this scopre
    }

    { // Scope 2; VALID becuse the borrowing is not mutable
        let s3 = &s;
        let s4 = &s;
    }

    // println!("\nDangling References");
    // let ptr = get_pointer();
    // let p; // ANOTHER EXAMPLE OF DANGLING REFERENCE
    // {
    //     let num = 10;
    //     p = &num;
    // }

    // println!("{}", p);  // This is invalid because the value being pointed by p is already freed
    //                     // when we went out of the scope of num

    println!("\n──────────────────────────");
    println!("Lifetimes");

    let v1 = vec![1, 2, 3];
    let v2 = vec![2, 3, 4, 5, 6, 7];
    let ptr = get_bigger(&v1, &v2); // let get_bigger borrow v1 and v2 because we'll use it below

    println!("Bigger of {:?} and {:?} is {:?}.", v1, v2, ptr);
}

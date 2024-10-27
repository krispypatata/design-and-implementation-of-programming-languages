fn main() {
    println!("══════════════════════════");
    println!("CONTROL FLOW STATEMENTS");

    println!("\n──────────────────────────");
    println!("Conditional Statements");

    let grade = 55;
    let lec = 6;
    let lab = 3;

    if (lec > 7) || (lab > 3) {
        println!("Grade is 5.0");
    } else if grade >= 55 {
        println!("You passed CMSC 124");
    } else {
        println!("Grade is 5.0");
    }

    let condition = true;
    let num = if condition { 5 } else { 6 };

    println!("{}", num);

    println!("\n──────────────────────────");
    println!("Iterative Statements");

    println!("\nLoop");
    // This repeats the statements inside the loop block and only terminates if it encounters a termination condition.

    let mut i = 0;

    loop {
        i += 1;
        println! {"{}", i};

        if i == 10 {
            break;
        }
    }

    println!("\nWhile");
    // This repeats the statements inside the while block and only terminates if the condition evaluates to false.

    let mut i = 0;
    
    while i != 10 {
        i += 1;
        println!("{}", i);
    }

    println!("\nFor");
    // The for loop can be used to iterate over a collection or a range. The break and continue keywords can also be used in for loops.

    for i in 1..11 {
        println!("{}", i);
    }

    let v1 = vec![1, 2, 3, 4, 5];

    // iterating over v1 , however , since v1 is not mutable
    // we cannot change the value of its elements

    for i in &v1 {
        println!("{}", i);
    }
    
    // iterating over a mutable vector and changing its contents
    let mut v2 = vec![1, 2, 3, 4, 5];

    for i in &mut v2 {  // & means i is just a reference to the original data
        *i = *i + 10;             // * accesses to values of a reference (like pointer)
    }

    println!("{:?}", v2);   // will print the new values

}

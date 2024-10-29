use std::error::Error; // Error class
use std::fs::File; // File class
use std::io::prelude::*; // for file reading (io)
use std::path::Path; // Path class

// ----------------------------------------------------------------------
// TO OPEN A FILE FOR READING (NO WRITING)
// ----------------------------------------------------------------------
// fn main() {
//     let path = Path::new("files/in.txt");
//     let display = path.display();

//     let mut file = match File::open(&path) {
//         // Description method of the error returned by open (if it fails)
//         // This clause will be used if an error occurs .
//         Err(why) => panic!(" couldn't open {}: {}", display, why.description()),
//         Ok(file) => file,
//         // If there are no problems opening the file (Ok) , then it is opened
//         // and accessible using the variable file
//     };

//     let mut s = String::new();
//     match file.read_to_string(&mut s) {
//         Err(why) => panic!("couldn't read {}: {}", display, why.description()),
//         Ok(_) => print!("{} contains :\n{}", display, s),
//         // s will print whole content of file
//     }

//     // file variable goes out of scope
//     // file automatically gets closed
// }

// ----------------------------------------------------------------------
// TO OPEN A FILE FOR WRITING (NO READING)
// ----------------------------------------------------------------------
fn main() {
    let path = Path::new("files/out.txt");
    let display = path.display();

    // creates a new file
    let mut file = match File::create(&path) {
        Err(why) => panic!("couldn't create {}: {}", display, why.description()),
        Ok(file) => file,
    };

    let mut s = "We love Rust!";
    // writes in the file
    match file.write_all(s.as_bytes()) {
        Err(why) => panic!("couldn't write {} to {}", why.description(), display),
        Ok(_) => println!("successfully wrote to {}", display),
    };

    // file variable goes out of scope
    // file automatically gets closed
}

// DESCRIPTION: This is a simple Rust program that reads an ArnoldC file (named 
// 'input.arnoldc') containing (not necessarily correct) ArnoldC code and analyzes 
// its contents to extract various elements, including numbers (integers), keywords,
// strings, and non-keyword identifiers. The program processes the file and outputs 
// the results to an 'output.txt' file in a well-organized format.
// AUTHOR:   Keith Ginoel S. Gabinete
// CREATED:  October 30, 2024
// ═════════════════════════════════════════════════════════════════════════════════════

extern crate regex;
use regex::Regex;
use std::fs::File; // File class
use std::io::prelude::*; // for file reading (io)
use std::io::{self, Write};
use std::path::Path; // Path class

// ═════════════════════════════════════════════════════════════════════════════════════
// Function for displaying the main menu
fn display_menu() {
    println!("==================== MENU ====================");
    println!("[1] Show all numbers");
    println!("[2] Show all keywords");
    println!("[3] Show all strings");
    println!("[4] Show all non-keyword identifiers");
    println!("[5] Exit");
    println!();
}

// ─────────────────────────────────────────────────────────────────────────────────────
// Helper function for getting a String input from the user
fn get_user_input(prompt: &str) -> String {
    // Will store input from the use
    let mut input = String::new();

    // Display the prompt message
    print!("{}", prompt);

    // Flush stdout to display the prompt immediately
    io::stdout().flush().unwrap();

    // Read the input from the user
    io::stdin()
        .read_line(&mut input)
        .expect("Failed to read Input!");

    // Return value
    input.trim().to_string()
}

// ─────────────────────────────────────────────────────────────────────────────────────
// Function for getting user choice based on the menu options
fn ask_choice(lower_bound: u8, upper_bound: u8) -> u8 {
    loop {
        // Ask input from the user
        let input = get_user_input("Enter choice: ");

        // Try to parse the read input
        // If input is invalid, default value is 0
        let choice: u8 = input.parse().unwrap_or(0);

        // Check if choice is within range
        if choice >= lower_bound && choice <= upper_bound {
            return choice; // Return value
        } else {
            // Error message
            println!("Invalid Input! \nPlease enter a number from {} to {} only.", lower_bound, upper_bound);
        }

        println!("");
    }
}


// ═════════════════════════════════════════════════════════════════════════════════════
// Function for reading contents from a file
fn read_from_file(file_path: &str, storage: &mut String) {
    let path = Path::new(file_path);
    let display = path.display();

    let mut file = match File::open(&path) {
        // Description method of the error returned by open (if it fails)
        // This clause will be used if an error occurs .
        Err(why) => panic!("Failed to open {}: {}", display, why),
        Ok(file) => file,
        // If there are no problems opening the file (Ok) , then it is opened
        // and accessible using the variable file
    };

    // Store the read contents to the passed 'storage' variable
    match file.read_to_string(storage) {
        Err(why) => panic!("Failed to read {}: {}", display, why),
        Ok(_) => println!("Successfully retrieved contents from {}.", file_path),
    }

    // file variable goes out of scope
    // file automatically gets closed
}


// ═════════════════════════════════════════════════════════════════════════════════════
// Function for writing contents to a file
fn write_to_file(file_path: &str, contents: &String) {
    let path = Path::new(file_path);
    let display = path.display();

    // creates a new file
    let mut file = match File::create(&path) {
        Err(why) => panic!("Failed to create {}: {}", display, why),
        Ok(file) => file,
    };

    // writes to file (passed 'contents' variable)
    match file.write_all(contents.as_bytes()) {
        Err(why) => panic!("Failed to write {} to {}.", why, display),
        Ok(_) => println!("Successfully wrote to {}.", display),
    };

    // file variable goes out of scope
    // file automatically gets closed
}

// ═════════════════════════════════════════════════════════════════════════════════════
// Function for extracting lines from file contents
fn extract_lines(text: &String) -> Vec<String> {
    // .lines()...=> Use lines() to iterate over lines directly
    // .filter... => Exclude empty lines or lines with only whitespace
    // .map... => Trim whitespace characters and convert the line to String
    // .collect() => Create a vector containing all the formatted lines
    let extracted_lines = text
        .lines()
        .filter(|line| !line.trim().is_empty())
        .map(|line| line.trim().to_string())
        .collect();

    // Print lines - For checking
    // for line in &extracted_lines {
    //     println!("{}", line);
    // }

    // Return value
    extracted_lines
}

// ═════════════════════════════════════════════════════════════════════════════════════
// Below are functions for each operation available in the main menu
// These functions will match regex expressions based on the user's choice
// ─────────────────────────────────────────────────────────────────────────────────────
fn get_all_numbers(lines: &Vec<String>) -> String {
    // Initialize the return value
    let mut result = String::new();

    // Regex pattern
    let pattern = "\\b\\d+\\b";
    let reg_ex = Regex::new(pattern).unwrap();

    // Vector to store unique detected matches
    let mut matches = Vec::new();

    // Iterate over the input lines vector
    for line in lines {
        // Iterate over the captured matches in the text/line
        for cap in reg_ex.captures_iter(line) {
            // Extract the detected match
            let detected_match = cap.at(0).unwrap().trim();

            // Check if the match is already in the vector before adding
            if !matches.contains(&detected_match) {
                matches.push(detected_match);
            }
        }
    }

    // Push the total number of unique matches
    result.push_str(&format!("Count:{}\n", matches.len()));

    // Iterate over the unique matches and append each to the result string
    for detected_matched in &matches {
        result.push_str(&format!("Detected integer: {}\n", detected_matched));
    }

    // Return value
    result
}

// ─────────────────────────────────────────────────────────────────────────────────────
fn get_all_keywords(lines: &Vec<String>) -> String {
    // Initialize the return value
    let mut result = String::new();

    // Regex pattern
    let pattern = "^[A-Z][A-Z\\s',]*";
    let reg_ex = Regex::new(pattern).unwrap();

    // Vector to store unique detected matches
    let mut matches = Vec::new();

    // Iterate over the input lines vector
    for line in lines {
        // Iterate over the captured matches in the text/line
        for cap in reg_ex.captures_iter(line) {
            // Extract the detected match
            let detected_match = cap.at(0).unwrap().trim();

            // Check if the match is already in the vector before adding
            if !matches.contains(&detected_match) {
                matches.push(detected_match);
            }
        }
    }

    // Push the total number of unique matches
    result.push_str(&format!("Count:{}\n", matches.len()));

    // Iterate over the unique matches and append each to the result string
    for detected_matched in &matches {
        result.push_str(&format!("Detected keyword: {}\n", detected_matched));
    }

    // Return value
    result
}

// ─────────────────────────────────────────────────────────────────────────────────────
fn get_all_strings(lines: &Vec<String>) -> String {
    // Initialize the return value
    let mut result = String::new();

    // Regex pattern
    let pattern = "\"(.*)\"";
    let reg_ex = Regex::new(pattern).unwrap();

    // Vector to store unique detected matches
    let mut matches = Vec::new();

    // Iterate over the input lines vector
    for line in lines {
        // Iterate over the captured matches in the text/line
        for cap in reg_ex.captures_iter(line) {
            // Extract the detected match
            let detected_match = cap.at(1).unwrap().trim();

            // Check if the match is already in the vector before adding
            if !matches.contains(&detected_match) {
                matches.push(detected_match);
            }
        }
    }

    // Push the total number of unique matches
    result.push_str(&format!("Count:{}\n", matches.len()));

    // Iterate over the unique matches and append each to the result string
    for detected_matched in &matches {
        result.push_str(&format!("Detected string literal: {}\n", detected_matched));
    }

    // Return value
    result
}

// ─────────────────────────────────────────────────────────────────────────────────────
fn get_all_non_keyword_identifiers(lines: &Vec<String>) -> String {
    // Initialize the return value
    let mut result = String::new();

    // Regex pattern - also ccepts macros (@NO PROBLEMO)
    let pattern = "([a-z_][a-zA-Z0-9_]*)|(@[A-Z][A-Z\\s',]*)";
    let reg_ex = Regex::new(pattern).unwrap();

    // For filtering out entries that are enclosed in double quotes (string literals)
    let excluded_reg_ex = Regex::new("\"(.*)\"").unwrap();

    // Vector to store unique detected matches
    let mut matches= Vec::new();

    // Iterate over the input lines vector
    for line in lines {
        // Remove string literals from the line for more accurate detection
        let temp_line = excluded_reg_ex.replace_all(line, "").to_string();

        // Iterate over the captured matches in the text/line
        for cap in reg_ex.captures_iter(&temp_line) {
            // Extract the detected match
            let detected_match = cap.at(0).unwrap().trim();

            // Check if the match is already in the vector before adding
            if !matches.contains(&detected_match.to_string()) {
                matches.push(detected_match.to_string());
            }
        }
    }

    // Push the total number of unique matches
    result.push_str(&format!("Count:{}\n", matches.len()));

    // Iterate over the unique matches and append each to the result string
    for detected_matched in &matches {
        result.push_str(&format!("Detected identifier: {}\n", detected_matched));
    }

    // Return value
    result
}


// ═════════════════════════════════════════════════════════════════════════════════════
// MAIN PROGRAM
fn main() {
    // Initialization of some necessary variables
    let mut output_contents = String::new();
    let mut input_contents = String::new();
    let input_lines;
    let mut choice:u8 = 0;

    // Read contents from a file
    read_from_file("files/input.arnoldc", &mut input_contents);

    // Extract the lines from the retrieved text (for easier regex matching)
    input_lines = extract_lines(&input_contents);

    // Loop the program until the user chooses to terminate it
    while choice != 5 {
    // Ask for user input
        display_menu();
        choice = ask_choice(1, 5);

        // Perform appropriate action based on user input
        if choice == 1 {
            output_contents = get_all_numbers(&input_lines);
        } else if choice == 2 {
            output_contents = get_all_keywords(&input_lines);
        } else if choice == 3 {
            output_contents = get_all_strings(&input_lines);
        } else if choice == 4 {
            output_contents = get_all_non_keyword_identifiers(&input_lines);
        } 
        
        if choice == 5 {
            print!("Goodbye!");
        } else {
            // Export the results into a file
            write_to_file("files/output.txt", &output_contents);
        }

        println!();
    }

    // For checking
    // print!("\n{}", &get_all_numbers(&input_lines));
    // print!("\n{}", &get_all_keywords(&input_lines));
    // print!("\n{}", &get_all_strings(&input_lines));
    // print!("\n{}", &get_all_non_keyword_identifiers(&input_lines));

    // print!("{}", &output_contents);
}

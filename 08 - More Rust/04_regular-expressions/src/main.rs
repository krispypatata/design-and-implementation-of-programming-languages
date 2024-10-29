extern crate regex;
use regex::Regex;

fn main() {
    let re = Regex::new(r"^\d{4}-\d{2}-\d{2}$").unwrap();
    assert!(re.is_match("2014-01-01")); // Successful if there's no error

    // ITERATING OVER CAPTURE GROUPS
    println!("Iterating Over Capture Groups");
    let re = Regex::new(r"(\d{4})-(\d{2})-(\d{2})").unwrap();
    // let text = "2012-03-14, 2013-01-01 and 2014-07-05";
    let text = "2012-03-14, 2013-01-01 and 2014-07-05";
    for cap in re.captures_iter(text) {
        println!(
            "Month: {} Day: {} Year: {}",
            cap.at(2).unwrap_or(""),
            cap.at(3).unwrap_or(""),
            cap.at(1).unwrap_or("")
        );
    }

    // Capture groups can also be named by using ?P<group name> inside its grouping.
    println!("\nNaming Capture Groups");
    let re = Regex::new(r"(?P<y>\d{4})-(?P<m>\d{2})-(?P<d>\d{2})").unwrap();
    let text = "2012-03-14, 2013-01-01 and 2014-07-05";
    for cap in re.captures_iter(text) {
        println!(
            "Month: {} Day: {} Year: {}",
            cap.name("m").unwrap_or(""),
            cap.name("d").unwrap_or(""),
            cap.name("y").unwrap_or("")
        );
    }
}

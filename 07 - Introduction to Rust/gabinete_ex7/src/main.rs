// DESCRIPTION: This is a simple rust program that simulates a ticketing system that 
// allows users to add events and enables customers to buy tickets for said events 
// (like smtickets). There is no limit to the number of artists and/or customers that
// can be added. The program also includes validation checks to ensure proper input
// processing from users, while also effectively handling potential errors.
// AUTHOR:   Keith Ginoel S. Gabinete
// CREATED:  October 27, 2024
// ═════════════════════════════════════════════════════════════════════════════════════
use std::io::{self, Write};

// ═════════════════════════════════════════════════════════════════════════════════════
// STRUCTURE DEFINITIONS
struct ArtistEvent {
    event_id: u32,
    event_title: String,
    artist: String,
    datetime: String,
    ticket_price: f64,
    stock: u32
}

struct Customer {
    name: String ,
    tickets_bought: Vec<String>,
    total_cost: f64
}


// ═════════════════════════════════════════════════════════════════════════════════════
// Function for displaying the main menu
fn display_menu() {
    println!("==================== MENU ====================");
    println!("[1] Add Event Details");
    println!("[2] Buy Ticket");
    println!("[3] Edit Event Details");
    println!("[4] Delete Event");
    println!("[5] View All Events");
    println!("[6] View All Customers");
    println!("[7] Exit");
    println!();
}

// ─────────────────────────────────────────────────────────────────────────────────────
// Helper function for getting a String input from the user
fn get_user_input(prompt: String) -> String {
    // Will store input from the use
    let mut input = String::new();

    // Display the prompt message
    print!("{}", prompt);

    // Flush stdout to display the prompt immediately
    io::stdout().flush().unwrap(); 

    // Read the input from the user
    io::stdin().read_line(&mut input).expect("Failed to read Input!");

    // Return value
    input.trim().to_string()
}

// ─────────────────────────────────────────────────────────────────────────────────────
// Function for getting user choice based on the menu options
fn ask_choice() -> u8 {
    loop {
        // Ask input from the user
        let input = get_user_input("Enter choice: ".to_string());

        // Try to parse the read input
        // If input is invalid, default value is 0
        let choice:u8 = input.parse().unwrap_or(0);

        // Check if choice is within range within range 1-7
        if choice >= 1 && choice <= 7 { 
            return choice;      // Return value
        } else {
            println!("Invalid Input! \nPlease enter a number between 1 and 7 only."); // Error message
        }

        println!("");
    }
}


// ═════════════════════════════════════════════════════════════════════════════════════
// Function for adding an event to the event records
fn add_event(events: &mut Vec<ArtistEvent>) {
    // Ask the required details of the event from the user
    let event_id = get_user_input("Event ID: ".to_string());

    // ---------------------------------------------------------------------------------
    // Validate Event ID
    let event_id = event_id.parse::<u32>();
    if event_id.is_err() {
        println!("\nInvalid Event ID: Must be a number!");
        return;
    } 

    // Extract/unwrap the valid value
    let event_id = event_id.unwrap(); 

    // Check if event id already exists
    // Exit the function if it is
    for event in &events[..] {
        if event.event_id == event_id {
            println!("\nEntered ID already exists!");
            return;
        }
    }
    // ---------------------------------------------------------------------------------

    let event_title = get_user_input("Event Title: ".to_string());
    let artist = get_user_input("Artist: ".to_string());
    let datetime = get_user_input("Date and Time: ".to_string());
    let ticket_price = get_user_input("Ticket Price: ".to_string());

    // ---------------------------------------------------------------------------------
    // Validate Ticket Price
    let ticket_price = ticket_price.parse::<f64>();
    if ticket_price.is_err() {
       println!("\nInvalid Ticket Price: Must be a decimal number!");
       return;
    }

    let stock = get_user_input("Stock: ".to_string());
    // ---------------------------------------------------------------------------------
    // Validate Stock
    let stock = stock.parse::<u32>();
    if stock.is_err() {
        println!("\nInvalid Stock: Must be a number!");
        return;
    }
    // --------------------------------------------------------------------------------

    // Create a new event (ArtistEvent) instance
    let new_event = ArtistEvent {
        event_id: event_id, // Use the unwrapped event_id here
        event_title: event_title,
        artist: artist,
        datetime: datetime,
        ticket_price: ticket_price.unwrap(),
        stock: stock.unwrap(),
    };

    // Add the new event to the events vector
    events.push(new_event);
    println!("\nSuccessfully added event!");
}


// ═════════════════════════════════════════════════════════════════════════════════════
// Function for allowing a customer to buy a ticket for the available existing events
fn buy_ticket(customers: &mut Vec<Customer>, events: &mut Vec<ArtistEvent>) {
    // Check first if the events collection is empty
    if events.is_empty() {
        println!("There are no events available!");
        return;
    }
    // ---------------------------------------------------------------------------------

    // Ask for the name of the customer
    let customer_name = get_user_input("Enter customer name: ".to_string());

    // --------------------------------------------------------------------------------
    // Show the events available
    println!();
    println!("-------- EVENTS AVAILABLE --------");
    for event in &events[..] {
        println!("\t[{}] {} ({}) - {}", event.event_id, event.event_title, event.artist, event.ticket_price);
    }
    println!();

    // Ask for the id of the event the customer wants to buy
    let event_id = get_user_input("Enter Event ID to buy: ".to_string());

    // --------------------------------------------------------------------------------
    // Validate Event ID
    let event_id = event_id.parse::<u32>();
    if event_id.is_err() {
        println!("\nInvalid Event ID: Must be a number!");
        return;
    }

    // Extract/unwrap the valid value
    let event_id = event_id.unwrap();

    // Check if the entered event id exists
    let mut is_existing: bool = false;
    let mut event_index: usize = 0; // To hold the index of the event's ticket to buy

    for (index, event) in events.iter().enumerate() {
        if event.event_id == event_id {
            is_existing = true;
            event_index = index; // Save the index
            break;
        }
    }

    if !is_existing {
        println!("\nEvent ID not in the list!");
        return;
    }
    // --------------------------------------------------------------------------------
    // Proceed to buying the event ticket if there's no error
    // Get mutable reference using the index
    let event_to_buy = &mut events[event_index];

    // Check if there's still stock of event tickets
    if event_to_buy.stock <= 0 {
        println!("\nEvent ticket is out of stock!");
        return;
    }

    // ---------------------------------------------------------------------------------
    // Process the request if there's still stock
    // Decrement stock of the event tickets
    event_to_buy.stock -= 1;

    // Check if the customer exists in the customer collection
    // If customer is already in the collection, just fetch it to edit its information
    let mut is_existing = false;
    let mut customer_index: usize = 0;
    for (index, customer) in customers.iter().enumerate() {
        if customer.name == customer_name {
            is_existing = true;
            customer_index = index;
            break;
        }
    }

   // Otherwise, create a new instance of customer and add it to the collection
   if !is_existing {
       customers.push(Customer {
           name: customer_name[..].to_string(),
           tickets_bought: Vec::new(),
           total_cost: 0.0,
       });
       customer_index = customers.len() - 1;
   }

    // Update the customer's total money spent
    let customer = &mut customers[customer_index];
    customer.total_cost += event_to_buy.ticket_price;

    // Add the newly bought ticket to the the customer's tickets bought collection
    let bought_ticket = format!("{}_{}_{}", event_to_buy.event_id, event_to_buy.event_title, event_to_buy.datetime);
    customer.tickets_bought.push(bought_ticket[..].to_string());

    println!("\nSuccessfully bought ticket {}!", bought_ticket);
}


// ═════════════════════════════════════════════════════════════════════════════════════
// Function to edit existing Event Details
fn edit_event(events: &mut Vec<ArtistEvent>) {
    // Check first if the events collection is empty
    if events.is_empty() {
        println!("There are no events yet!");
        return;
    }

    // ---------------------------------------------------------------------------------
    // Ask for the id of the event that needs editing
    let event_id = get_user_input("Enter Event ID: ".to_string());

    // ---------------------------------------------------------------------------------
    // Validate Event ID
    let event_id = event_id.parse::<u32>();
    if event_id.is_err() {
        println!("\nInvalid Event ID: Must be a number!");
        return;
    }

    // Extract/unwrap the valid value
    let event_id = event_id.unwrap();

    // Check if event id exists
    let mut is_existing: bool = false;
    let mut event_index: usize = 0; // To hold the index of the event to edit

    for (index, event) in events.iter().enumerate() {
        if event.event_id == event_id {
            is_existing = true;
            event_index = index; // Save the index
            break;
        }
    }

    if !is_existing {
        println!("\nEvent ID not found!");
        return;
    }
    // ---------------------------------------------------------------------------------
    // Proceed to other fields if found
    let new_datetime = get_user_input("Enter new date and time: ".to_string());

    let new_ticket_price = get_user_input("Enter new price: ".to_string());
    // ---------------------------------------------------------------------------------
    // Validate Ticket Price
    let new_ticket_price = new_ticket_price.parse::<f64>();
    if new_ticket_price.is_err() {
        println!("\nInvalid Ticket Price: Must be a decimal number!");
        return;
    }

    let new_stock = get_user_input("Enter new stock: ".to_string());
    // ---------------------------------------------------------------------------------
    // Validate Stock
    let new_stock = new_stock.parse::<u32>();
    if new_stock.is_err() {
        println!("\nInvalid Stock: Must be a number!");
        return;
    }
    // --------------------------------------------------------------------------------
    // Proceed to updating the event details if there's no error
    // Get mutable reference using the index
    let event_to_edit = &mut events[event_index];
    
    // Update the event's date and time, ticket price, and stock
    event_to_edit.datetime = new_datetime;
    event_to_edit.ticket_price = new_ticket_price.unwrap();
    event_to_edit.stock = new_stock.unwrap();

    println!("\nEvent Details Successfully Edited!");
}


// ═════════════════════════════════════════════════════════════════════════════════════
// Function to delete an event from the events collection
fn delete_event(events: &mut Vec<ArtistEvent>) {
    // Check first if the events collection is empty
    if events.is_empty() {
        println!("There are no events yet!");
        return;
    }

    // Ask for the id of the event that needs to be deleted
    let event_id = get_user_input("Enter Event ID: ".to_string());

    // Validate Event ID
    let event_id = event_id.parse::<u32>();
    if event_id.is_err() {
        println!("\nInvalid Event ID: Must be a number!");
        return;
    }

    // Extract/unwrap the valid value
    let event_id = event_id.unwrap();

    // Check if event id exists
    let mut is_existing: bool = false;
    let mut event_index: usize = 0; // To hold the index of the event that will be deleted

    for (index, event) in events.iter().enumerate() {
        if event.event_id == event_id {
            is_existing = true;
            event_index = index; // Save the index
            break;
        }
    }

    if !is_existing {
        println!("\nEvent ID not found!");
        return;
    }

    // Delete the event from the collection
    events.remove(event_index);

    println!("\nSuccessfully deleted event detail!");
}


// ═════════════════════════════════════════════════════════════════════════════════════
// Function to display the contents of the events collection
fn view_all_events(events: &Vec<ArtistEvent>) {
    // Check first if the events collection is empty
    if events.is_empty() {
        println!("There are no events yet!");
        return;
    }

    // Iterate through the collection
    for (i, event) in events.iter().enumerate() {
        println!("Event ID: {}", event.event_id);
        println!("Event Title: {}", event.event_title);
        println!("Artist: {}", event.artist);
        println!("Date and Time: {}", event.datetime);
        println!("Ticket Price: {}", event.ticket_price);
        println!("Stock: {}", event.stock);
    
        // Print a new line after each iteration (except for the last one)
        if i < events.len() - 1 {
            println!();
        }
    }
}


// ═════════════════════════════════════════════════════════════════════════════════════
// Function to display the contents of the customers collection
fn view_all_customers(customers: &Vec<Customer>) {
    // Check first if the customers record is empty
    if customers.is_empty() {
        println!("There are no customers yet!");
        return;
    }

    // Iterate through the collection
    for (i, customer) in customers.iter().enumerate() {
        println!("Customer Name: {}", customer.name);
        println!("Tickets Bought:");
        for ticket in &customer.tickets_bought[..] {
            println!("\t- {}", ticket);
        }
        println!("Total Cost: {}", customer.total_cost);
    
        // Print a new line after each iteration (except for the last one)
        if i < customers.len() - 1 {
            println!();
        }
    }
}


// ═════════════════════════════════════════════════════════════════════════════════════
fn main() {
    // Initialization of some necessary varioables for the program
    let mut events: Vec<ArtistEvent> = Vec::new();
    let mut customers: Vec<Customer> = Vec::new();
    let mut choice = 0;

    // Loop the program until the user chooses to terminate it
    while choice != 7 {
        display_menu();
        choice = ask_choice();
        println!();

        if choice == 1 {
            add_event(&mut events);
        } else if choice == 2 {
            buy_ticket(&mut customers, &mut events);
        } else if choice == 3 {
            edit_event(&mut events);
        } else if choice == 4 {
            delete_event(&mut events);
        } else if choice == 5 {
            view_all_events(&events);
        } else if choice == 6 {
            view_all_customers(&customers);
        } else if choice == 7 {
            print!("Goodbye!");
        }

        println!();
    }
}

% Filename should be: catsNC.erl
- module ( catsnc ) . % filename
- compile ( export_all ) . % tells which functions to compile

cat () ->
    receive % waits for the mailbox to have a message
    come_here ->
        io:format (" Shut up , human. ~n") ;
    catfood ->
        io:format (" Thank you! ~n") ;
    _ -> %if message is not come_here or not catfood
        io:format ("I’m your master. ~n")
    end.
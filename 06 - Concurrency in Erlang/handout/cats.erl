-module(cats).
-compile([nowarn_export_all, export_all]).

cat() -> 
    receive
        come_here ->
            io:format("Shut up, human. ~n");
        catfood ->
            io:format("Thank you! ~n");
        _ ->
            io:format("I'm your master. ~n")
end.

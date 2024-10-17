-module(cats2).
-compile([nowarn_export_all, export_all]).

cat() ->
    receive
        {From, come_here} -> 
            From !  "Shut up human. ~n",
            cat();
        {From, catfood} ->
            From ! "Thank you! ~n";
        _ ->
            io:format("I'm your master.~n"),
            cat()
end.
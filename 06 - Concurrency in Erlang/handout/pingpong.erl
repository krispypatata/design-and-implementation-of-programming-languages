-module(pingpong).
-compile([nowarn_export_all, export_all]).

start_pong() ->
    register(pong, spawn(pingpong, pong, [])).

pong() ->
    receive
        {ping, Ping_Pid} ->
            io:format("Pong got ping ~n"),
            Ping_Pid ! pong,
            pong();
        finished ->
            io:format("Pong finished ~n"),
            self() ! finished;
        _ ->
            pong() % Ignore other messages and continue
    end.

start_ping(Pong_Node) ->
    spawn(pingpong, ping, [Pong_Node]).

ping(Pong_Node) ->
    Input = io:get_line("Type 'ping' or 'finished': "),
    case Input of
        "ping\n" ->
            {pong, Pong_Node} ! {ping, self()},
            receive 
                pong -> 
                    io:format("Ping got pong ~n"),
                    ping(Pong_Node)
            end;
        "finished\n" ->
            io:format("Ping finished ~n"),
            self() ! finished;
        _ ->
            io:format("Invalid input. Try again.~n"),
            ping(Pong_Node)
    end.

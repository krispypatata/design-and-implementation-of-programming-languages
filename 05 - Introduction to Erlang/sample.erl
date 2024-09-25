-module(sample).
-export([hello/0, add/2, both/1, check/1]).

hello() ->
    io:format("Hello World!~n").

add(X,Y) ->
    X + Y.

both(X) ->
    hello(),
    add(5,X).

check(X) ->
    if
        X >= 7 -> io:format("X is 7!!!~n");
        X =:= 5 -> io:format("X is 5!!!~n")
    end.
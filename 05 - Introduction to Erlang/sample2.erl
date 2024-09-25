-module(sample2).
-export([greet/2, factorial/1, check/1, choice/1]).

greet(male, Name) ->
    % for males
    io:format("Hi Mr. ~s! ~n",[Name]),
    io:format("Have a good day ~n ");
greet(female, Name) ->
    % for females
    io:format ("Hi Ms. ~s! ~n" ,[Name]),
    io:format ("Have a good day ~n");
greet(_, Name) ->
    % default
    io:format("Hi ~s! ~n" ,[Name]),
    io:format("Have a good day ~n").

factorial(N) when N>0 ->
    N*factorial(N-1);
factorial(0) -> 1.

check(X) ->
    if
        X >= 7 -> io:format("X is 7!!!~n");
        X =:= 5 -> io:format("X is 5!!!~n")
    end.

choice(X) ->
    case X of
        1 -> io:format("Gryffindor!");
        2 -> io:format("Slytherin!");
        3 -> io:format("Ravenclaw!");
        4 -> io:format("Hufflepuff!");
        _ -> io:format("Muggle!")
    end.
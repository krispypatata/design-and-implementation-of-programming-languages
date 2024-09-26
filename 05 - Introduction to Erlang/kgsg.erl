% DESCRIPTION: 
%       This is a simple Erlang program designed to perform various numerical operations 
% on a list of integers. This program includes key functions for filtering prime numbers
% in the list, reversing the list, and calculating the product of the elements of the list
% while adhering to certain conditions (as explained further in the code).
% AUTHOR:   Keith Ginoel S. Gabinete
% CREATED:  September 27, 2024
% ═════════════════════════════════════════════════════════════════════════════════════

-module(kgsg).
-export([reverseList/1, isPrime/1, primeFilter/1, listMult/1, test/0]).

% ═════════════════════════════════════════════════════════════════════════════════════
% Function to check if a given number is prime
isPrime(N) -> 
    case abs(N) of
        0 -> false;
        1 -> false;
        _ when N - trunc(N) == 0 -> isPrime(abs(trunc(N)),2); % Make sure that N is a whole number
        % trunc() removes trailing decimals [ref: https://www.erlang.org/docs/26/man/erlang#trunc-1]
        % I know that the primeFilter function requires a list of integers as parameter, so it's unnecessary to check if N is a whole number here; but I'd like to perfect this isPrime function anyway
        _ -> false % Float numbers that are not whole numbers
end.
isPrime(N,N) -> true;
isPrime(N,Divisor)->
  case N rem Divisor of
    0 -> false;
    _ -> isPrime(N, Divisor + 1)
end.

% ─────────────────────────────────────────────────────────────────────────────────────
% Tail-recursive function to filter prime numbers from a list.
primeFilterTR([], FilteredList) -> FilteredList;    % Base case
primeFilterTR([Head | Tail], FilteredList) ->       % Recursive case
    case isPrime(Head) of
        true -> primeFilterTR(Tail, [Head | FilteredList]);
        false -> primeFilterTR(Tail, FilteredList) 
end.

% Helper function
primeFilter (NumList) ->
    primeFilterTR(NumList , [])
.


% ═════════════════════════════════════════════════════════════════════════════════════
% Tail-recursive function for reversing the contents of a list
reverseListTR([], ReversedList) -> ReversedList;    % Base case
reverseListTR([Head | Tail], ReversedList) ->       % Recursive case
    reverseListTR(Tail, [Head | ReversedList]).

% Helpter function
reverseList (NumList) ->
    reverseListTR(NumList, [])
.


% ═════════════════════════════════════════════════════════════════════════════════════
% Recursive function for computing the product of the contents of a list
% Conditions:
%   if product length is ODD, product should be NEGATIVE
%   if product length is EVEN, product should be POSITIVE
%   Would only get the product of the first 5 elements
%   In case, there are less than five elements, the product of the contents of the list should still be returned
%   Empty input list results to a product of 0
listMultR([], _, Product) ->    % Reaching end of list
    Product;
listMultR([Head | Tail], Index, Product) ->
    % Only get the product of the first five elements
    case Index =< 5 of
        true ->
            % Check if Index is Even or Odd
            case Index rem 2 of
                1 -> listMultR(Tail, Index + 1, -1 * Head * Product );  % Odd:  Negate the element before multiplying to the Product
                _ -> listMultR(Tail, Index + 1, Head * Product)         % Even: Multiply as is
            end;
        false -> Product
    end
.

% Helper function
listMult([]) -> % Case when input list is empty; product is zero
    0;
listMult (NumList) ->
    listMultR(NumList, 1, 1)
.


% ═════════════════════════════════════════════════════════════════════════════════════
% For testing the functions above
test() ->
    io:format("═══════════════════════════════~n"),
    io:format("1. Prime Filter~n"),
    io:format("primeFilter([3, 6, 9, 1, 2]).~n"),
    io:format("result: ~p ~n", [primeFilter([3, 6, 9, 1, 2])]),
    io:format("~n"),
    io:format("primeFilter([10, 6, 9, -3, 8]).~n"),
    io:format("result: ~p ~n", [primeFilter([10, 6, 9, -3, 8])]),
    io:format("~n"),
    io:format("primeFilter([10, 6, 9, 20, 8]).~n"),
    io:format("result: ~p ~n", [primeFilter([10, 6, 9, 20, 8])]),
    io:format("~n"),
    io:format("primeFilter([10, 83, 67, 29, 100]).~n"),
    io:format("result: ~p ~n", [primeFilter([10, 83, 67, 29, 100])]),
    io:format("~n"),

    io:format("═══════════════════════════════~n"),
    io:format("2. Reverse List~n"),
    io:format("reverseList([3, 6, 9, 1, 2]).~n"),
    io:format("result: ~p ~n", [reverseList([3, 6, 9, 1, 2])]),
    io:format("~n"),
    io:format("reverseList([8, 7, 6, 5, 4, 4]).~n"),
    io:format("result: ~p ~n", [reverseList([8, 7, 6, 5, 4, 4])]),
    io:format("~n"),
    
    io:format("═══════════════════════════════~n"),
    io:format("3. List Multiplication~n"),
    io:format("listMult([3, 6, 9, 1, 2]).~n"),
    io:format("result: ~p ~n", [listMult([3, 6, 9, 1, 2])]),
    io:format("~n"),
    io:format("listMult([3, 6, 9, 1]).~n"),
    io:format("result: ~p ~n", [listMult([3, 6, 9, 1])]),
    io:format("~n"),
    io:format("listMult([3]).~n"),
    io:format("result: ~p ~n", [listMult([3])]),
    io:format("~n"),
    io:format("listMult([]).~n"),
    io:format("result: ~p ~n", [listMult([])]),
    io:format("═══════════════════════════════~n"),
    io:format("~n")
.

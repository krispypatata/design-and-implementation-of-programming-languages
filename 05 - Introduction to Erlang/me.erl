-module(me).
-export([main/1]).

getSum(N) when N>0 ->
    N + getSum(N-1);
getSum(0) -> 0.

main(N) -> 
    getSum(N)
.



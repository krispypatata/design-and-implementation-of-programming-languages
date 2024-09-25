-module(metail).
-export([main/1]).

getSum(N, Acc) when N>0 ->
    getSum(N-1,Acc+N);
getSum(0,Acc) -> Acc.

main(N) -> 
    getSum(N, 0)
.



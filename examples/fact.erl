-module(fact).
-export([fact/1, main/0]).
fact(N) when N =< 1 -> 1;
fact(N) -> N * fact(N-1).
main() -> fact(10).

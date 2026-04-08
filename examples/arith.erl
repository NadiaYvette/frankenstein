-module(arith).
-export([sq/1, main/0]).
sq(X) -> X * X.
main() -> sq(5) + sq(6).

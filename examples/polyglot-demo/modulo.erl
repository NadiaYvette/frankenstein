-module(modulo).
-export([modulo/2]).
modulo(A, B) -> A - (A div B) * B.

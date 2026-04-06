:- module arith.
:- interface.
:- import_module int.

% Pure arithmetic for bridge bisimulation testing.
% Expected: compute(2) = 2 * (3 + 4) + 5 * 6 = 44
:- func compute(int) = int.

:- implementation.
compute(X) = X * (3 + 4) + 5 * 6.

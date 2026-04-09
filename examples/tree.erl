-module(tree).
-export([main/0]).

%% A tiny binary tree ADT.
%% Constructors:  leaf            (nullary)
%%                {node, V, L, R} (int value + two subtrees)

sum_tree(leaf)            -> 0;
sum_tree({node, V, L, R}) -> V + sum_tree(L) + sum_tree(R).

main() ->
    sum_tree({node, 2,
                {node, 1, leaf, leaf},
                {node, 3, leaf, leaf}}).

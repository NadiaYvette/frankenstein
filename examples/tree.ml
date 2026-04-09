type tree = Leaf | Node of int * tree * tree

let rec sum_tree t =
  match t with
  | Leaf -> 0
  | Node (v, l, r) -> v + sum_tree l + sum_tree r

let () = print_int (sum_tree (Node (2, Node (1, Leaf, Leaf), Node (3, Leaf, Leaf))))

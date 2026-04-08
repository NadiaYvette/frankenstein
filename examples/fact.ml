let rec fact n = if n <= 1 then 1 else n * fact (n - 1)
let () = print_int (fact 10)

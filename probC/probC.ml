open Format

(** INPUT *)

let check_tuples = function
    | [a; b; c; d] -> [(a, b); (c, d)]
    | _ -> invalid_arg "Incorrect number of arguments, you need 4"

let square = read_int ()

let inequalities = read_int ()

let rec ineq_fun = function
    | 0 -> []
    | n -> ((read_line () |> String.split_on_char ' ' |> List.map int_of_string) |> check_tuples) :: ineq_fun (n - 1)

let ineq_list = inequalities |> ineq_fun

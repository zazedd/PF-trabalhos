open Format

(** INPUT *)

let square = read_int () |> (fun x -> if (x >= 4 && x <= 6) then x else invalid_arg "Board must be from 4x4 to 9x9.")

let inequalities = read_int ()

let is_inside lst = List.for_all (fun x -> (x >= 0 && x < square)) lst

let is_neighbour = function
    | [a; b; c; d] -> begin
        if (a = c && (b = d + 1 || b = d - 1))
        || (b = d && (a = c + 1 || a = c - 1))
            then [(a, b); (c, d)]
            else invalid_arg ("Conditions not set correctly, the coordinates aren't neighbours")
        end
    | _ -> []

let check_ineq_input = function
    | [a; b; c; d] as lst -> if is_inside lst then is_neighbour lst 
                             else  
                                invalid_arg ("Conditions not set correctly, the coordinates aren't inside the board.")
    | _ -> invalid_arg "Incorrect number of arguments, you need 4"

let rec ineq_fun = function
    | 0 -> []
    | n -> ((read_line () |> String.split_on_char ' ' |> List.map int_of_string) |> check_ineq_input) :: ineq_fun (n - 1)


let board = Array.make_matrix square square 0

let print_board arr = Array.iter (fun y -> Array.iter (Printf.printf "%d ") y; print_newline ()) arr

let () = let lin = board.(1) in lin.(2) <- 213

let ineq_list = inequalities |> ineq_fun

let () = print_board board

(* let board = make_empty_board square

let () = print_board board *)
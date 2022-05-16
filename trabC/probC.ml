open Format

(** INPUT *)

let square = read_int () |> (fun x -> if (x >= 4 && x <= 9) then x else invalid_arg "Board must be from 4x4 to 9x9.")

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

let rec make_empty_board = function
    | 0 -> []
    | n -> begin
        let rec make_empty_line = function
            | 0 -> []
            | m -> 0 :: make_empty_line (m - 1)
            in
            make_empty_line square :: make_empty_board (n - 1)
        end

let rec print_board = function
    | [] -> ()
    | x :: xs -> List.iter (fun y -> Printf.printf "%d " y) x;
                 Printf.printf "\n";
                 print_board xs

let ineq_list = inequalities |> ineq_fun

let board = make_empty_board square

let () = print_board board
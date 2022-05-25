open Format

(** Input *)

let square = read_int () |> (fun x -> if (x >= 4 && x <= 6) then x else invalid_arg "Board must be either 4x4, 5x5 or 6x6.")

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


(** Solving the problem *)

let print_board brd = Array.iteri (fun i elem -> if (i + 1) mod square <> 0 then Printf.printf "%d " elem else Printf.printf "%d\n" elem) brd

let remove_in_lst lst rem = List.fold_left (fun xs x -> if List.mem x rem then xs else x :: xs) [] lst

let rec upto n = if n <= 0 then [] else n :: upto (n - 1)

let get arr (x, y) = arr.(x * square + y)

let next (x, y) = if y < square - 1 then (x, y + 1) else (x + 1, 0)

let is_valid elem = elem <> 0

let board_with_val brd num (x, y) =
    let brdcopy = Array.copy brd in
    brdcopy.(x * square + y) <- num;
    brdcopy

let rec get_row brd pos (x, _) = 
    if pos > square - 1 then []
    else
        get brd (x, pos) :: get_row brd (pos + 1) (x, 0)

let rec get_col brd pos (_, y) = 
    if pos > square - 1 then []
    else
        get brd (pos, y) :: get_col brd (pos + 1) (0, y)
        
let available_numbers brd (x, y) =
    let unavail = get_row brd 0 (x, y) @ get_col brd 0 (x, y) in
    let avail = upto square in
        print_board brd; (remove_in_lst avail unavail) |> List.fast_sort compare

let rec fill brd ((x, y) as pos) =
    if y > square then Some brd (* filled all entries *)
    else if is_valid (get brd pos) then fill brd (next pos)
    else 
        match available_numbers brd pos with
        | [] -> None (* no solution *)
        | l -> try_vals brd pos l
    and try_vals brd pos = function
        | [] -> None
        | v :: l -> begin
            Printf.printf "%d\n" v;
            match fill (board_with_val brd v pos) (next pos) with
            | Some _ as res -> res
            | None -> try_vals brd pos l
            end

let solve brd = 
    match fill brd (0, 0) with
    | Some sol -> sol
    | None -> failwith "IMPOSSIBLE\n"

(** chamadas finais e criações de variaveis *)

(* let print_board brd = Array.iteri (fun i elem -> if (i + 1) mod square <> 0 then Printf.printf "%d " elem else Printf.printf "%d\n" elem) brd *)

(** representação row-major, iniciado com 0s *)
let board = Array.make (square * square) 0 

let () = board.(1) <- 4

(* let () = board.(4) <- 3; board.(5) <- 2; board.(6) <- 0; board.(7) <- 0;
         board.(2) <- 4; board.(10) <- 0; board.(14) <- 0

let t = available_numbers board (1, 2)

let () = List.iter (Printf.printf "%d ") t; Printf.printf "\n" *)

let ineq_list = inequalities |> ineq_fun

let () = solve board |> print_board




(** true if safe, false if not *)
(* let is_safe num = function
    | (x, y) -> begin
        let rec is_safe_col num lin = function
            | (x, y) -> board.(lin).(y) = num || (if lin - 1 < 0 then false else is_safe_col num (lin - 1) (x, y))
            in
        not (Array.exists (fun a -> a = num) board.(x) || is_safe_col num (square - 1) (x, y))
    end *)

(* let rec solve_lin n pos lin brd ineq_lst =
    if n > square then solve_lin 1 pos lin brd ineq_lst
    else
    if pos + 1 > square then true
    else
    if is_safe n lin brd 
    then begin
        lin.(pos) <- n;
        if check_ineqs brd ineq_lst then solve_lin (n + 1) (pos + 1) lin brd ineq_lst
        else begin
            lin.(pos) <- 0;
            solve_lin (n + 1) pos lin brd ineq_lst
        end
    end        
    else
        solve_lin (n + 1) pos lin brd ineq_lst *)
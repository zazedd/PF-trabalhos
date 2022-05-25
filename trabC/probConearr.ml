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


(** RESOLUÇÂO *)

(** Função remove_in_lst: Remove da lista lst os elementos presentes em rem *)
let remove_in_lst lst rem = List.fold_left (fun xs x -> if List.mem x rem then xs else x :: xs) [] lst

(** Função count_upto: Cria a lista de números possíveis até square (com o 0)
*   [0; 1; 2; ... square]                                               
*)
let rec count_upto n = if n <= 0 then [] else n :: count_upto (n - 1)

let rec count_from n = if n > square then [] else n :: count_from (n + 1)

(** Função get: 
*   Como o array está em row-major, usamos a formula para chegar ao 
*   elemento correspondente a uma certa coordenada. Esta função retorna o elemento
*   da coordenada (x, y).
 *)
let get arr (x, y) = arr.(x * square + y)

(** Função next:  *)
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

let rec check_ineq brd (x, y) = function
    | [] -> []
    | i :: rest -> begin
                let rec check_ineq_aux = function
                | [(a, b); (c, d)] -> if (x, y) = (a, b) then 
                                      (let smaller = get brd (c, d) in
                                        if smaller = 0 then []
                                        else count_upto smaller)
                                      else if (x, y) = (c, d) then
                                      (let bigger = get brd (a, b) in
                                        if bigger = 0 then []
                                        else count_from bigger)
                                      else
                                        check_ineq brd (x, y) rest
                | _ -> assert false
                in
                check_ineq_aux i
            end
      
 (* let unavail = get_row brd 0 (x, y) @ get_col brd 0 (x, y) @ check_ineq brd (x, y) ineq_list in *)       
let available_numbers brd ineq_list (x, y) =
    let test1 = get_row brd 0 (x, y) in
    let test2 = get_col brd 0 (x, y) in
    let test3 = check_ineq brd (x, y) ineq_list in
    let unavail  = test1 @ test2 @ test3 in
    (if (x, y) = (3, 1) then (* List.iter (Printf.printf "%d ") test1; Printf.printf "\n";
                              List.iter (Printf.printf "%d ") test2; Printf.printf "\n"; *)
                               (* List.iter (Printf.printf "%d ") test3; Printf.printf "\n"; *)
                                List.iter (Printf.printf "%d ") unavail; Printf.printf "\n";
                                Printf.printf "---------\n");
    let avail = count_upto square in
        (remove_in_lst avail unavail) |> List.fast_sort compare

let rec fill brd ineq_list ((x, y) as pos) =
    if pos = (square, 0) then Some brd                          (** todas as posições já estão preenchidas *) 
    else 
        match available_numbers brd ineq_list pos with
        | [] -> None                                            (** se não houver numeros disponíveis, não há solução nesta branch *)
        | l -> try_vals brd ineq_list pos l
    and try_vals brd ineq_list pos = function
        | [] -> None
        | v :: l -> begin
            match fill (board_with_val brd v pos) ineq_list (next pos) with
            | Some _ as res -> res
            | None -> try_vals brd ineq_list pos l
            end

let print_board brd = Array.iteri (fun i elem -> if (i + 1) mod square <> 0 then Printf.printf "%d " elem else Printf.printf "%d\n" elem) brd

let solve brd ineq_list = 
    match fill brd ineq_list (0, 0) with
    | Some sol -> print_board sol
    | None -> Printf.printf "IMPOSSIBLE\n"


(** Chamadas finais e criações de variaveis *)

(** Representação do tabuleiro em row-major, iniciado com 0s *)
let board = Array.make (square * square) 0 

(* let () = board.(9) <- 1 *)

let ineq_list = inequalities |> ineq_fun

(* let () = List.iter (Printf.printf "%d ") (check_ineq board (3, 1) ineq_list); Printf.printf "\n"
let () = List.iter (Printf.printf "%d ") (check_ineq board (3, 1) ineq_list); Printf.printf "\n"
let () = List.iter (Printf.printf "%d ") (check_ineq board (3, 1) ineq_list); Printf.printf "\n" *)


let () = solve board ineq_list

open Format

(** Input *)

let square = read_int () |> (fun x -> if (x >= 4 && x <= 6) then x else invalid_arg "Board must be either 4x4, 5x5 or 6x6.")

let inequalities = read_int ()

let is_inside lst = List.for_all (fun x -> (x >= 0 && x < square)) lst

let check_ineq_input = function
    | [a; b; c; d] -> [(a, b); (c, d)]
    | _ -> invalid_arg "Incorrect number of arguments, you need 4"

let rec ineq_fun = function
    | 0 -> []
    | n -> ((read_line () |> String.split_on_char ' ' |> List.map int_of_string) |> check_ineq_input) :: ineq_fun (n - 1)

let sort_ineq_list_aux lst1 lst2 = 
    match lst1, lst2 with
    | [(a, b); (c, d)], [(e, f); (h, i)] -> let num1 = int_of_string (string_of_int a ^ string_of_int b ^ string_of_int c ^ string_of_int d) in
                                            let num2 = int_of_string (string_of_int e ^ string_of_int f ^ string_of_int h ^ string_of_int i) in
                                            compare num1 num2
    | _ -> assert false

let sort_ineq_list lst = List.sort sort_ineq_list_aux lst

(** RESOLUÇÂO *)

(** Função remove_in_lst: Remove da lista lst os elementos presentes em rem *)
let remove_in_lst lst rem = List.fold_left (fun xs x -> if List.mem x rem then xs else x :: xs) [] lst

let ( -!) = remove_in_lst

(** Função count_upto: Cria a lista de números possíveis até square (com o 0)
*   [0; 1; 2; ... square]                                               
*)
let rec count_upto n = if n < 0 then [] else n :: count_upto (n - 1)

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
                                        else count_upto smaller @ check_ineq brd (x, y) rest)
                                      else if (x, y) = (c, d) then
                                      (let bigger = get brd (a, b) in
                                        if bigger = 0 then []
                                        else count_from bigger @ check_ineq brd (x, y) rest)
                                      else
                                        check_ineq brd (x, y) rest
                | _ -> assert false
                in
                check_ineq_aux i
            end

      
let print_board brd = Array.iteri (fun i elem -> if (i + 1) mod square <> 0 then Printf.printf "%d " elem else Printf.printf "%d\n" elem) brd

(* let unavail = get_row brd 0 (x, y) @ get_col brd 0 (x, y) @ check_ineq brd (x, y) ineq_list in *)       
let available_numbers brd ineq_list (x, y) =
    let unavail = (get_row brd 0 (x, y)) @ (get_col brd 0 (x, y)) @ (check_ineq brd (x, y) ineq_list) in
    let avail = count_upto square in
        (avail -! unavail)

let rec insert brd ineq_list ((x, y) as pos) =
    if pos = (square, 0) then Some brd                          (** todas as posições já estão preenchidas *) 
    else
        match available_numbers brd ineq_list pos with
        | [] -> None                                            (** se não houver numeros disponíveis, não há solução nesta branch *)
        | avail -> try_insert brd ineq_list pos avail
    and try_insert brd ineq_list pos = function
        | [] -> None
        | n :: rest -> begin
            match insert (board_with_val brd n pos) ineq_list (next pos) with
            | Some _ as res -> res
            | None -> try_insert brd ineq_list pos rest
            end

let solve brd ineq_list = 
    match insert brd ineq_list (0, 0) with
    | Some sol -> print_board sol
    | None -> Printf.printf "IMPOSSIBLE\n"


(** Chamadas finais e criações de variaveis *)

(* let timeS = Sys.time () *)

(** Representação do tabuleiro em row-major, iniciado com 0s *)
let board = Array.make (square * square) 0 

let ineq_list = inequalities |> ineq_fun |> sort_ineq_list

let () = solve board ineq_list

(* let () = Printf.printf "%.40f\n" (Sys.time () -. timeS) *)

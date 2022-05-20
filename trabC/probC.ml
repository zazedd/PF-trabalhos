open Format

module S = Set.Make(struct type t = int let compare = compare end)

let rec upto n = if n < 0 then S.empty else S.add n (upto (n-1))

let map f s = S.fold (fun x s -> S.add (f x) s) s S.empty

let make_setlist set1 set2 = set1 :: set2 :: []

(** INPUT *)

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

(** se colocarmos um numero num certo quadrado do tabuleiro, a linha e a coluna dessa coordenada ficam indisponiveis. *)

let board = Array.make_matrix square square 0

(** d1 -> linhas indisponiveis para um certo numero n
    d2 -> colunas indisponiveis para um certo numero n *)
let rec solve n d1 d2 = function
    | [lin; col] -> begin
                    if n > square then true
                    else
                        if S.is_empty lin || S.is_empty col then solve (n + 1) S.empty S.empty (make_setlist (upto (square - 1)) (upto (square - 1)))
                        else
                            let x = S.min_elt (S.diff lin d1) 
                            and y = S.min_elt (S.diff col d2) in
                            let test = make_setlist (S.remove x lin) (S.remove y col)
                            in
                            board.(x).(y) <- n;
                            solve n S.empty S.empty test
                end
                    
    | _ -> assert false

let print_board arr = Array.iter (fun y -> Array.iter (Printf.printf "%d ") y; print_newline ()) arr

let ineq_list = inequalities |> ineq_fun

let yes = solve 1 S.empty S.empty (make_setlist (upto (square - 1)) (upto (square - 1)))

let () = print_board board



(** true if safe, false if not *)
(* let is_safe num = function
    | (x, y) -> begin
        let rec is_safe_col num lin = function
            | (x, y) -> board.(lin).(y) = num || (if lin - 1 < 0 then false else is_safe_col num (lin - 1) (x, y))
            in
        not (Array.exists (fun a -> a = num) board.(x) || is_safe_col num (square - 1) (x, y))
    end *)
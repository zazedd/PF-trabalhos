open Format

type t = 
  | Leaf
  | Node of t * int * t * int (* left node, value, right node, height *)

(** retorna a altura do node lmao *)
let height = function
  | Leaf -> 0
  | Node (_, _, _, h) -> h

let empty = Leaf

(** faz um node na arvore *)
let make_node l v r = Node (l, v, r, 1 + max (height l) (height r))

(** roda o node para a direita *)
let rot_right = function
  | Node (Node (l1, v1, r1, _), v2, r2, _) ->
      make_node l1 v1 (make_node r1 v2 r2)
  | _ -> failwith "rotation on leaf is not supported"

(** mm merda para a esquerda*)
let rot_left = function
  | Node (l1, v1, Node (l2, v2, r1, _), _) ->
      make_node (make_node l1 v1 l2) v2 r1
  | _ -> failwith "rotation on leaf is not supported"

(** mete os valores dos nodes por onde a funcao andou numa lista ate encontrar o numero pedido x*)
let rec get_path x = function
  | Node (l, v , r, _) -> begin
      match compare x v with
      | 0 -> [x]                                        (** está contido, acabamos o caminho com o proprio numero*)
      | result when result < 0 -> v :: get_path x l     (** x < v, continuar a procura à esquerda e acrescentamos o valor do node ao caminho *)
      | _ -> v :: get_path x r                          (** x > v, continuar a procura à direita e acrescentamos o valor do node ao caminho *)
  end
  | _ -> []

let rec insert x = function
  | Leaf -> Node (Leaf, x, Leaf, 1)                     (** caso inicial *)
  | Node (l, v, r, _) as current -> begin
      match compare x v with
      | 0 -> current
      | result when result < 0 -> begin                 (** x < v *)
          match insert x l with
          | Node (l2, v2, r2, h2) as nextl -> begin
              match (h2 - height r) <= 1 with
              | true -> make_node nextl v r
              | false -> let nextl = if height l2 < height r2 then rot_left nextl
                         else nextl in
                         rot_right (make_node nextl v r)
              end 
          | Leaf -> failwith "insertion on leaf isn't supported"
          end
      | _ -> begin                                      (** x > v *)
          match insert x r with
          | Node (l2, v2, r2, h2) as nextr -> begin
              match (h2 - height l) <= 1 with
              | true -> make_node l v nextr
              | false -> let nextr = if height l2 < height r2 then rot_right nextr
                         else nextr in
                         rot_left (make_node l v nextr)
              end
          | Leaf -> failwith "insertion on leaf isn't supported"
          end
      end

(** tail recursive *)
let rec make_list = function
  | 0 -> []
  | n -> read_int () :: make_list (n - 1)

(** tail recursive *)
let rec make_big_list = function 
  | 0 -> []
  | n -> (read_int () |> make_list) :: make_big_list (n - 1)

let num_trees = read_int ()

(** lista de lista dos inputs *)
let inputlst = num_trees |> make_big_list

let rec print_lsts = function
  | [] -> ()
  | x :: xs -> List.iter (Printf.printf "| %d\n") (List.sort_uniq compare x); 
               Printf.printf "\n"; 
               print_lsts xs 

let rec make_tree = function
  | [] -> []
  | x :: xs -> List.fold_left (fun t a -> insert a t) empty x :: make_tree xs

let tree_lst = make_tree inputlst

let () = print_lsts inputlst

let timestart = Sys.time ()

let () = List.iter (Printf.printf "%d ") (get_path 0 (List.hd tree_lst))
let () = Printf.printf "\n"
let () = List.iter (Printf.printf "%d ") (get_path 2 (List.hd tree_lst))
let () = Printf.printf "\n"
let () = List.iter (Printf.printf "%d ") (get_path 3 (List.hd tree_lst))
let () = Printf.printf "\n"
let () = List.iter (Printf.printf "%d ") (get_path 4 (List.hd tree_lst))
let () = Printf.printf "\n"
let () = List.iter (Printf.printf "%d ") (get_path 5 (List.hd tree_lst))
let () = Printf.printf "\n"
let () = List.iter (Printf.printf "%d ") (get_path 6 (List.hd tree_lst))
let () = Printf.printf "\n"
let () = List.iter (Printf.printf "%d ") (get_path 9 (List.hd tree_lst))
let () = Printf.printf "\n"
let () = List.iter (Printf.printf "%d ") (get_path 10 (List.hd tree_lst))
let () = Printf.printf "\n"

let timeend = Sys.time ()

let () = Printf.printf "%.40f\n" (timeend -. timestart)

(*https://coq.inria.fr/library/Coq.MSets.MSetAVL.html*)

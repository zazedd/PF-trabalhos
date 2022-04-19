open Format

type t = 
  | Leaf
  | Node of t * int * t * int (* left node, value, right node, height *)

(** retorna a altura do node lmao *)
let height = function
  | Leaf -> 0
  | Node (_, _, _, h) -> h

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

(** validacao simples dos inputs dos elem das arvores*)
let validate x = if x < 0 || x > 10000 then invalid_arg "Invalid number of elements in tree (0-10000)" else x

(** tail recursive *)
let rec make_list = function
  | 0 -> []
  | n -> read_int () :: make_list (n - 1)

(** tail recursive *)
let rec make_big_list = function 
  | 0 -> []
  | n -> (validate (read_int ()) |> make_list) :: make_big_list (n - 1)

let num_trees = 
  let tmp = read_int () in 
  if tmp < 0 || tmp > 5000 then invalid_arg "Invalid number of trees (0-5000)" else tmp

(** lista de lista dos inputs *)
let list = num_trees |> make_big_list

let timestart = Sys.time ()

let () = List.iter (Printf.printf "| %d\n") (List.fast_sort compare (List.nth list 0))
let () = Printf.printf "\n"
let () = List.iter (Printf.printf "| %d\n") (List.fast_sort compare (List.nth list 1))
let () = Printf.printf "\n"
let () = List.iter (Printf.printf "| %d\n") (List.fast_sort compare (List.nth list 2))

let timeend = Sys.time ()

let () = Printf.printf "%.40f" (timeend -. timestart)

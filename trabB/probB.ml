open Format

type t = 
  | Leaf
  | Node of t * int * t * int (* left node, value, right node, height *)

(** returns height of node lmao *)
let height = function
  | Leaf -> 0
  | Node (_, _, _, h) -> h

(** makes a node in the tree *)
let make_node l v r = Node (l, v, r, 1 + max (height l) (height r))

(** rotates the tree to the right on a specific node (the one thats passed) *)
let rot_right = function
  | Node (Node (l1, v1, r1, _), v2, r2, _) ->
      make_node l1 v1 (make_node r1 v2 r2)
  | _ -> failwith "rotation on leaf is not supported"

(** same shit but left *)
let rot_left = function
  | Node (l1, v1, Node (l2, v2, r1, _), _) ->
      make_node (make_node l1 v1 l2) v2 r1
  | _ -> failwith "rotation on leaf is not supported"

(** using quick sort (partitioning and pivotal comparison) to sort list *)
let rec quicksort = function
  | [] -> []
  | x :: xs -> begin
    let sml, lrg = List.partition (fun y -> y < x) xs
    in quicksort sml @ (x :: quicksort lrg)
  end

(** simple validation of first input *)
let validate x = if x < 0 || x > 10000 then invalid_arg "Invalid number of elements in tree (0-10000)" else x

(** tail recursive *)
let rec make_list = function
  | 0 -> []
  | n -> read_int () :: make_list (n - 1)

(** tail recursive *)
let rec make_big_list = function 
  | 0 -> []
  | n -> (validate (read_int ()) |> make_list) :: make_big_list (n - 1)

let num_trees = read_int ()

let list = 
  if num_trees < 0 || num_trees > 5000 then invalid_arg "Invalid number of trees (0-5000)" 
  else num_trees |> make_big_list

let timestart = Sys.time ()

let () = List.iter (Printf.printf "| %d\n") (quicksort (List.nth list 0))
let () = Printf.printf "\n"
let () = List.iter (Printf.printf "| %d\n") (quicksort (List.nth list 1))
let () = Printf.printf "\n"
let () = List.iter (Printf.printf "| %d\n") (quicksort (List.nth list 2))

let timeend = Sys.time ()

let () = Printf.printf "%.40f" (timeend -. timestart)

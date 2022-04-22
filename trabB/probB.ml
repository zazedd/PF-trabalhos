open Format

type t = 
  | Leaf
  | Node of t * int * t * int (* left node, value, right node, height *)

let empty = Leaf


(** FUNCOES DE ARVORES*)
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

(** mete os valores dos nodes por onde a funcao andou numa lista ate encontrar o numero pedido x*)
let rec get_path x = function
  | Node (l, v , r, _) -> begin
      match compare x v with
      | 0 -> [x]                                        (** está contido, acabamos o caminho com o proprio numero*)
      | result when result < 0 -> v :: get_path x l     (** x < v, continuar a procura à esquerda e acrescentamos o valor do node ao caminho (lista) *)
      | _ -> v :: get_path x r                          (** x > v, continuar a procura à direita e acrescentamos o valor do node ao caminho (lista) *)
  end
  | _ -> []

let rec insert x = function
  | Leaf -> Node (Leaf, x, Leaf, 1)                     (** caso inicial / base*)
  | Node (l, v, r, _) as current -> begin
    match compare x v with
    | 0 -> current
    | result when result < 0 -> begin                 (** x < v *)
      match insert x l with
      | Node (l2, v2, r2, h2) as nextl -> begin
        match (h2 - height r) <= 1 with
        | true -> make_node nextl v r
        | false -> let nextl = if height l2 < height r2 then rot_left nextl  (**este then idealmente nunca será executado. apenas para programacao defesa *)
                   else nextl in
                   rot_right (make_node nextl v r)
        end 
      | Leaf -> failwith "insertion on leaf isn't supported"
      end
    | _ -> begin                                     (** x > v (este caso nunca deve ser chamado pois a lista entra ordenada, por isso x nunca e maior que v)*)
      match insert x r with                        (** basicamente so para a programacao defensiva do programa *)   
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

(** faz a lista de arvores*)
let rec make_tree = function
  | [] -> []
  | x :: xs -> List.fold_left (fun t y -> insert y t) empty x :: make_tree xs

(** assegura que o programa continua apenas se as arvores estiverem balancadas *)
let rec assert_balance lst =
  let rec assert_balance_aux = function
    | Leaf -> ()
    | Node (l, _, r, h) -> begin
        assert (h = 1 + max (height l) (height r));
        assert_balance_aux r;
        assert_balance_aux l;
    end
  in
  match lst with
  | [] -> ()
  | x :: xs -> assert_balance_aux x; assert_balance xs




(** FUNCOES DE INPUT PRINCIPAL*)
(** remove duplicados de uma lista. é rapida porque fold_left é tail-rec*)
let remove_dups xs = List.fold_left (fun xs x -> if List.mem x xs then xs else x :: xs) [] xs

(** funcao que faz a lista de elementos consoante o numero destes. é tail recursive *)
let rec make_list = function
  | 0 -> []
  | n -> read_int () :: make_list (n - 1)

(** funcao que faz a lista de lista de elementos consoante o numero de arvores. trata tambem do sorting destes. é tail recursive *)
let rec make_big_list = function 
  | 0 -> []
  | n -> (List.fast_sort compare (read_int () |> make_list |> remove_dups) |> List.rev) :: make_big_list (n - 1)

let rec print_lsts = function
  | [] -> ()
  | x :: xs -> List.iter (Printf.printf "| %d\n") x; 
               Printf.printf "\n"; 
               print_lsts xs 

(*let rec first_common = function
  | (x, y) -> begin
    match *)

let rec exec_get_path a b = function
  | [] -> []
  | x :: xs -> begin
    let path1 = get_path a x |> List.rev in
      match compare (List.hd path1) a with
      | 0 -> begin
        let path2 = get_path b x |> List.rev in
          match compare (List.hd path2) b with
          | 0 -> (List.find (fun y -> List.mem y path2) path1 |> string_of_int) :: exec_get_path a b xs
          | _ -> "NO" :: exec_get_path a b xs
      end
      | _ -> "NO" :: exec_get_path a b xs 
    end

(** CHAMADAS DE FUNCOES*)
(** lista de lista dos inputs *)
let inputlst = read_int () |> make_big_list 

(** nodes que queremos ver *)
let (mutation1, mutation2) = Scanf.scanf "%d %d" (fun x y -> (x ,y))

let () = print_lsts inputlst

(** faz uma lista das arvores *)
let tree_lst = make_tree inputlst

(** assegura que todas as arvores estão balancadas *)
let () = assert_balance tree_lst

let () = List.iter (Printf.printf "%s\n") (exec_get_path mutation1 mutation2 tree_lst)



let () = List.iter (Printf.printf "%d ") (get_path 1 (List.hd tree_lst))
let () = Printf.printf "\n"
let () = List.iter (Printf.printf "%d ") (get_path 3 (List.hd tree_lst))
let () = Printf.printf "\n"
let () = List.iter (Printf.printf "%d ") (get_path 4 (List.hd tree_lst))
let () = Printf.printf "\n"
let () = List.iter (Printf.printf "%d ") (get_path 6 (List.hd tree_lst))
let () = Printf.printf "\n"
let () = List.iter (Printf.printf "%d ") (get_path 7 (List.hd tree_lst))
let () = Printf.printf "\n"
let () = Printf.printf "\n"

let () = List.iter (Printf.printf "%d ") (get_path 1 (tree_lst |> List.rev |> List.hd))
let () = Printf.printf "\n"
let () = List.iter (Printf.printf "%d ") (get_path 2 (tree_lst |> List.rev |> List.hd))
let () = Printf.printf "\n"
let () = List.iter (Printf.printf "%d ") (get_path 3 (tree_lst |> List.rev |> List.hd))
let () = Printf.printf "\n"
let () = List.iter (Printf.printf "%d ") (get_path 5 (tree_lst |> List.rev |> List.hd))
let () = Printf.printf "\n"
let () = List.iter (Printf.printf "%d ") (get_path 6 (tree_lst |> List.rev |> List.hd))
let () = Printf.printf "\n"


(*let timestart = Sys.time ()
let timeend = Sys.time ()
let () = Printf.printf "%.40f\n" (timeend -. timestart)*)

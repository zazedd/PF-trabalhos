open Format

type t = 
  | Leaf
  | Node of t * int * t * int (* node da esquerda, valor, node da direita, altura *)

let empty = Leaf



(** FUNCOES DE ARVORES *)

(** Retorna a altura do node em questão: *)
let height = function
  | Leaf -> 0
  | Node (_, _, _, h) -> h

(** Faz um node usando um valor e os dois nodes necessários à sua criação *)
let make_node l v r = Node (l, v, r, 1 + max (height l) (height r))

(** Roda a àrvore para a direita no node em questão: 
*
*   Basta pegar no node existente e no seu respetivo filho da esquerda,
*   e fazer com que esse filho seja o parente na nova estrutura.
*)
let rot_right = function
  | Node (Node (l1, v1, r1, _), v2, r2, _) ->
      make_node l1 v1 (make_node r1 v2 r2)
  | _ -> failwith "Right rotation on leaf is not supported."

(** Roda a àrvore para a esquerda no node em questão: 
*
*   Basta pegar no node existente e no seu respetivo filho da direita,
*   e fazer com que esse filho seja o parente na nova estrutura.
*)
let rot_left = function
  | Node (l1, v1, Node (l2, v2, r1, _), _) ->
      make_node (make_node l1 v1 l2) v2 r1
  | _ -> failwith "Left rotation on leaf is not supported."

(** Faz uma lista dos valores no caminho tomado para chegar a x, começando no node principal 
*   
*   Se o numero x não existir na árvore, a função executa na mesma e dá return
*   ao caminho que seria ter sido percorrido se este lá estivesse. No entanto o
*   último elemento do caminho difere de x, o que perimite à função "common_mutation"
*   perceber que este valor não está na árvore.
*)
let rec get_path x = function
  | Node (l, v , r, _) -> begin
      match compare x v with
      | 0 -> [x]                                        (** está contido, acabamos o caminho com o proprio numero*)
      | result when result < 0 -> v :: get_path x l     (** x < v, continuar a procura à esquerda e acrescentamos o valor do node ao caminho (lista) *)
      | _ -> v :: get_path x r                          (** x > v, continuar a procura à direita e acrescentamos o valor do node ao caminho (lista) *)
  end
  | _ -> []

(** Função responsável pela inserção e balanço dos valores na árvore: *)
let rec insert x = function
  | Leaf -> Node (Leaf, x, Leaf, 1)                     (** caso inicial / base*)
  | Node (l, v, r, _) as current -> begin
    match compare x v with
    | 0 -> current                                      (** x = v, (apenas para defensão) não é preciso fazer nada, apenas retornamos o node que entrou *)
    | result when result < 0 -> begin                   (** x < v, vamos inserir o node à esquerda, possívelmente vai ser preciso rotações para balançar *)
      match insert x l with
      | Node (l2, v2, r2, h2) as nextl -> begin         
        match (h2 - height r) <= 1 with                 (** Se a diferença altura do node que acabamos de inserir e a altura do node da direita do parente for menor ou igual a 1: *)
        | true -> make_node nextl v r                   (** Não é preciso balanço, e apenas inserimos o node a esquerda do parente *)
        | false -> let nextl = if height l2 < height r2 then rot_left nextl   
                   else nextl in                                              (** Caso contrário, dependendo das alturas, aplicamos a rotação correspondente a descrepância *)
                   rot_right (make_node nextl v r)
        end 
      | Leaf -> failwith "Insertion on leaf is not supported."
      end
    | _ -> begin                                        (** x > v, vamos inserir o node à direita, possívelmente vai ser preciso rotações para balançar *)
      match insert x r with                         
      | Node (l2, v2, r2, h2) as nextr -> begin
        match (h2 - height l) <= 1 with                 (** Se a diferença altura do node que acabamos de inserir e a altura do node da esquerda do parente for menor ou igual a 1: *)
        | true -> make_node l v nextr                   (** Não é preciso balanço, e apenas inserimos o node a direita do parente *)
        | false -> let nextr = if height r2 < height l2 then rot_right nextr
                   else nextr in                                              (** Caso contrário, dependendo das alturas, aplicamos a rotação correspondente a descrepância *)
                   rot_left (make_node l v nextr)
        end
      | Leaf -> failwith "Insertion on leaf is not supported."
      end
  end

(** Faz a lista de arvores: 
*
*   Vai inserindo os valores pela esquerda. Começa com uma árvore vazia
*)
let rec make_tree = function
  | [] -> []
  | x :: xs -> List.fold_left (fun t y -> insert y t) empty x :: make_tree xs

(** Assegura que o programa continua apenas se as arvores estiverem balancadas 
*
*   Itera pelos nodes todos da árvore, assegurando-se que a altura nunca é maior do que 1 / -1
*)
let rec assert_balance lst =
  let rec assert_balance_aux = function
    | Leaf -> ()
    | Node (l, _, r, h) -> begin
        assert (h = 1 + max (height l) (height r));     (** Se a altura não for igual a 1, 0 ou -1, o programa aborta *)
        assert_balance_aux r;                           (** Continua o check á direita *)
        assert_balance_aux l;                           (** e á esquerda *)
    end
  in
  match lst with
  | [] -> ()
  | x :: xs -> assert_balance_aux x; assert_balance xs  (** Itera pelas árvores *)



(** FUNCOES DE INPUT/OUTPUT*)

(** Remove valores duplicados de uma lista. Fold_left é tail-rec e por isso ocupa heap-space constante*)
let remove_dups xs = List.fold_left (fun xs x -> if List.mem x xs then xs else x :: xs) [] xs

(** Faz a lista de elementos consoante o número destes. É tail recursive *)
let rec make_list = function
  | 0 -> []
  | n -> read_int () :: make_list (n - 1)

(** Faz a lista de lista de elementos consoante o número de arvores. É tail recursive *)
let rec make_big_list = function 
  | 0 -> []
  | n -> (read_int () |> make_list |> remove_dups) :: make_big_list (n - 1)

(** Encontra o primeiro elemento comum entre as listas *)
let first_common lst1 lst2 = List.find (fun y -> List.mem y lst2) lst1

(** Responsável pela chamada da função get_path e tratamento do seu output consoante o ultimo input:
*
*   Retorna uma lista de strings (onde o número de elementos é o mesmo que o número de árvores), que podem ser:
*   - "NO", se não existir uma ou as duas "mutações" pedidas na árvore
*   - Um inteiro (em string), que corresponde ao primeiro node comum entre as "mutações"
*
*   Obs: os caminhos são invertidos porque eles começam com o node principal. Como queremos o primeiro node comum,
*   revertemos a ordem.
*)
let rec common_mutation a b = function
  | [] -> []
  | x :: xs -> begin
    let path1 = get_path a x |> List.rev in             
      match compare (List.hd path1) a with              (** O ultimo elemento do caminho para a mutação1 é igual si mesmo? *)
      | 0 -> begin                                      
        let path2 = get_path b x |> List.rev in
          match compare (List.hd path2) b with                               (** O ultimo elemento do caminho para a mutação2 é igual si mesmo? *)
          | 0 -> (first_common path1 path2) :: common_mutation a b xs        (** Se sim, encontramos o primeiro elemento comum nos camingos e inserimos-lo na lista. Continuamos com a proxima árvore *)
          | _ -> common_mutation a b xs                                      (** Se não, como a mutação2 não está na árvore, continuamos com a proxima árvore. *)
      end
      | _ -> common_mutation a b xs                     (** Como a mutação1 não está na árvore, continuamos com a proxima arvore. *)
    end

(** Reponsável pelo handling do output: 
* Se nenhuma árvore apresentar mutações comuns mete "NO" no stdout, caso contrario 
* mete as mutações em comum.
*)
let rec print_common_mutations = function
  | [] -> Printf.printf "NO\n"
  | x :: xs -> begin
    match xs with
    | [] -> Printf.printf "%d\n" x
    | y :: ys -> Printf.printf "%d\n" x; print_common_mutations xs
    end

(** CHAMADAS DE FUNCOES*)

(** Lista de lista dos inputs *)
let inputlst = read_int () |> make_big_list 

(** Nodes que queremos comparar nas árvores *)
let (mutation1, mutation2) = Scanf.scanf "%d %d" (fun x y -> (x ,y))

(** Lista das arvores *)
let tree_lst = (make_tree inputlst) |> List.rev

(** Assegura que todas as arvores estão balançadas *)
let () = assert_balance tree_lst

(** Output das mutações em comum *)
let () = print_common_mutations (common_mutation mutation1 mutation2 tree_lst)



(** FUNÇÔES DE DEBUGGING *)
(*let rec print_lsts = function
  | [] -> ()
  | x :: xs -> List.iter (Printf.printf "| %d\n") x; 
               Printf.printf "\n"; 
               print_lsts xs  *)

(** let () = print_lsts inputlst *)

(*let timestart = Sys.time ()
let timeend = Sys.time ()
let () = Printf.printf "%.40f\n" (timeend -. timestart)*)
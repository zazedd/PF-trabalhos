open Format

(** Input *)

(** Tamanho do tabuleiro *)
let square = read_int () |> (fun x -> if (x >= 4 && x <= 6) then x else invalid_arg "Tabuleiro tem de ser 4x4, 5x5 ou 6x6.")

(** Função de check e tratamento do input para as inequações *)
let check_ineq_input = function
    | [a; b; c; d] -> [(a, b); (c, d)]
    | _ -> invalid_arg "Número incorreto de elementos (4)."

(** Função que cria uma lista de listas de tuplas do input das inequações *)
let rec ineq_fun = function
    | 0 -> []
    | n -> ((read_line () |> String.split_on_char ' ' |> List.map int_of_string) |> check_ineq_input) :: ineq_fun (n - 1)

(** Função para sortear por ordem lexicográfica a lista dos inputs: 
*   
*   Esta função é um pouco abusada. Faz uso do facto de que se "juntarmos" os números todos
*   pertencentes a duas tuplas (Ex: (1, 0) e (2, 3) -> 1023), e fizermos o mesmo para outro conjunto de tuplas,
*   conseguimos saber quem é maior lexicográficamente apenas comparando os dois números finais.
*   É necessário porque a ordem das inequações importa para a função check_ineq
*)
let sort_ineq_lst_aux lst1 lst2 = 
    match lst1, lst2 with
    | [(a, b); (c, d)], [(e, f); (h, i)] -> let num1 = int_of_string (string_of_int a ^ string_of_int b ^ string_of_int c ^ string_of_int d) in
                                            let num2 = int_of_string (string_of_int e ^ string_of_int f ^ string_of_int h ^ string_of_int i) in
                                            compare num1 num2
    | _ -> assert false

(** Aplica a função acima à lista pretendida *)
let sort_ineq_lst lst = List.sort sort_ineq_lst_aux lst


(** Resolução *)

(** Remove da lista lst os elementos presentes em rem *)
let remove_in_lst lst rem = List.fold_left (fun xs x -> if List.mem x rem then xs else x :: xs) [] lst

(** Função count_upto: 
*   Cria uma lista de números ordenados até square (com o 0) -> [0; 1; 2; ... square]                                               
*)
let rec count_upto n = if n < 0 then [] else n :: count_upto (n - 1)

(** Função count_from: 
*   Cria uma lista de números ordenados a partir de n até square -> [n; n + 1; ... square]                                               
*)
let rec count_from n = if n > square then [] else n :: count_from (n + 1)

(** Função get: 
*   Como o array está em row-major, usamos a formula (x * square + y) para chegar ao 
*   elemento correspondente a uma certa coordenada. Esta função retorna o elemento
*   no array correspondente ao elemento da coordenada (x, y) de um tabuleiro com lado square.
 *)
let get arr (x, y) = arr.(x * square + y)

(** Função next: Dá a coordenada horizontalmente seguinte a (x, y) tendo em conta o tamanho do tabuleiro. *)
let next (x, y) = if y < square - 1 then (x, y + 1) else (x + 1, 0)

(** Função board_with_val: 
*   Faz uma cópia do tabuleiro e insere o elemento num na posição (x, y) desta cópia
*)
let board_with_val brd num (x, y) =
    let brdcopy = Array.copy brd in
    brdcopy.(x * square + y) <- num;
    brdcopy

(** Função get_row: 
*   Coloca numa lista os elementos da linha correspondente à posição (x, y)
*   Estes serão os números indisponíveis para qualquer outra casa na mesma linha
*)
let rec get_row brd pos (x, _) = 
    if pos > square - 1 then []
    else
        get brd (x, pos) :: get_row brd (pos + 1) (x, 0)

(** Função get_col: 
*   Coloca numa lista os elementos da coluna correspondente à posição (x, y)
*   Estes serão os números indisponíveis para qualquer outra casa na mesma coluna
*)
let rec get_col brd pos (_, y) = 
    if pos > square - 1 then []
    else
        get brd (pos, y) :: get_col brd (pos + 1) (0, y) 

(** Função check_ineq: 
*   Devolve uma lista com os números indísponiveis para uma casa (x, y) por não verificarem as inequações
*)
let rec check_ineq brd ((x, y) as pos) = function
    | [] -> []
    | i :: rest -> begin
                let rec check_ineq_aux = function
                | [(a, b); (c, d)] -> if pos = (a, b) then                                  (** Se a casa é o elemento maior: *)
                                      (let smaller = get brd (c, d) in
                                        if smaller = 0 then []                              (** e o elemento menor for 0, não podemos concluir nada (podem ser todos os números) *)
                                        else count_upto smaller @ check_ineq brd pos rest)  (** caso contrário nao podem ser os numeros até o valor pequeno (inclusive) *)
                                      else if pos = (c, d) then                             (** Se a casa é o elemento menor: *)
                                      (let bigger = get brd (a, b) in                       (** e o elemento maior for 0, não podemos concluir nada *)
                                        if bigger = 0 then []                               (** caso contrário nao podem ser os numeros a partir do maior *)
                                        else count_from bigger @ check_ineq brd pos rest)
                                      else
                                        check_ineq brd (x, y) rest                          (** Se a casa não for nenhum dos elementos continu a o check com o resto das inequações *)
                | _ -> assert false
                in
                check_ineq_aux i
            end


(** Função available_numbers: 
*   Devolve uma lista com os números disponíveis para uma casa (x, y)
*)
let available_numbers brd ineqlst ((x, y) as pos) =
    let unavail = (get_row brd 0 pos) @ (get_col brd 0 pos) @ (check_ineq brd pos ineqlst) in (** todos os números indisponíveis, seja por estarem na mesma coluna. linha, ou não responderem à inequação *)
    let avail = count_upto square in                            (** todos os números disponíveis, sem nenhumas restrições *)
        remove_in_lst avail unavail                             (** a diferença simétrica dá os números disponíveis para a posição *)

(** Função insert: 
*   Faz a colocação dos números no tabuleiro. Funciona por backtracking/brute-force.
*)
let rec insert brd ineqlst ((x, y) as pos) =
    if pos = (square, 0) then Some brd                          (** todas as posições já estão preenchidas, devolve o tabuleiro resolvido *) 
    else
        match available_numbers brd ineqlst pos with            (** Há valores disponíveis para esta casa? *)
        | [] -> None                                            (** não há, por isso não há solução nesta branch *)
        | avail -> try_insert brd ineqlst pos avail             (** existe pelo menos um valor por isso vamos tentar colocá-lo *)
    and try_insert brd ineqlst pos = function                   
        | [] -> None                                            (** caso chegue ao fim da lista de números disponíveis e nenhum teve solução, então neste branch não há mais soluções (backtracking) *)
        | n :: rest -> begin
            match insert (board_with_val brd n pos) ineqlst (next pos) with (** Mete um valor disponível no tabuleiro (copia), e verifica se há solução neste branch: *)
            | Some _ as res -> res                              (** se sim, perfeito, achou a solução *)
            | None -> try_insert brd ineqlst pos rest           (** se não, continua com o resto dos números disponíveis *)
            end

(** Função print_board: Imprime o tabuleiro para o stdout *)
let print_board brd = Array.iteri (fun i elem -> if (i + 1) mod square <> 0 then Printf.printf "%d " elem else Printf.printf "%d\n" elem) brd

(** Função solve: 
*   Trata dos casos com e sem solução. Caso haja, imprimime o tabuleiro resolvido, caso não, imprimime "IMPOSSIBLE"
*)
let solve brd ineqlst = 
    match insert brd ineqlst (0, 0) with
    | Some sol -> print_board sol
    | None -> Printf.printf "IMPOSSIBLE\n"


(** Chamadas finais e criações de variaveis *)

(** Representação do tabuleiro em row-major, iniciado com 0s *)
let board = Array.make (square * square) 0 

let ineqlst = read_int () |> ineq_fun |> sort_ineq_lst

let () = solve board ineqlst



(** Exemplo: 
*
*   In:
*   6
*   12
*   0 4 1 4
*   1 2 1 3
*   4 1 4 2
*   2 0 1 0
*   0 5 1 5
*   3 0 4 0
*   3 2 3 3
*   4 2 3 2
*   2 1 2 0    
*   3 3 4 3
*   5 2 5 3
*   5 5 5 4
*
*   Out:
*   1 2 3 4 5 6
*   2 3 6 5 1 4
*   4 5 1 6 2 3
*   5 1 4 3 6 2
*   3 6 5 2 4 1
*   6 4 2 1 3 5
*
*   +- 0.03s na minha máquina
*
*)

open Format

let timeStart = Sys.time ()

(**Definição das funções "infix" usadas da biblioteca zarith*)
let zi x = Z.of_int x
let ( *!) = Z.mul
let ( /!) = Z.div
let ( +!) = Z.add
let ( -!) = Z.sub

(**Definição das constantes para uso nas funções da biblioteca zarith*)
let zero = Z.zero
let one = Z.one
let two = Z.succ one
let three = Z.succ two
let six = zi 6

(**Ler o input da consola: 
* 
*  1 - Lê a linha do stdin depois do Enter
*  2 - "Parte" a string em subpartes delimitadas por espaços
*  3 - Usa as substrings para criar uma lista de inteiros
*)
let args = read_line () |> String.split_on_char ' ' |> List.map int_of_string

(**Função checks: 
* 
*  1 - Se a lista tiver exatamente 2 elementos, verifica se estes estão dentro dos valores
*      pedidos no enunciado. Se tal se confirmar, nada acontece, caso contrário levanta uma excepção.
*  2 - Se a lista tiver qualquer outra estrutura, levanta uma excepção, pois não temos exatamente 2 
*       elementos para usar.
*)
let checks = function
  | [a; b] -> if (a < 0 || a > 20) then invalid_arg "First element not in range (0-20)" 
    else if (b < 0 || b > 10000) then invalid_arg "Second element not in range (0-10000)" 
  | _ -> invalid_arg "Invalid number of arguments, you need 2"

let () = checks args


(**Hashtables: 
* 
*  1 - Cria uma hashtable com tamanho igual ao segundo input do utilziador
*  2 - Inicia as duas primeiras posições com as condições iniciais do problema
*  
*  A hashtable comentada abaixo é a necessária para a implementação da 2a parte com options
*)
let table = Hashtbl.create (List.nth args 1)
let () = Hashtbl.add table 0 one; 
         Hashtbl.add table 1 two

(*let table : (int, Z.t option) Hashtbl.t = Hashtbl.create (List.nth args 1)
let () = Hashtbl.add table 0 (Some one);
         Hashtbl.add table 1 (Some two)*)


(**Função sum (helper da equação 1): 
* 
*  Calcula a parte do sumatório da primeira equação.
*  Input:
*     n - Primeiro input dado pelo utilziador (a) 
*     k - Controlador do sumatório 
*     f - A função sobre a qual vamos aplicar o sumatório (neste caso sn1)
*  
*  Output:
*     Tupla onde o primeiro elemento é o resultado do sumatório e o segundo é
*     o número de vezes que a função f foi chamada.
*)
let rec sum n k f =
  if k > (n - 2) then 0, 0
  else 
    let (a, count1), (b, count2), (sumRest, sumCount) = f (k), f (n - k - 1), sum n (k + 1) f in
      a * b + sumRest, count1 + count2 + sumCount

(**Função sn1 (equação 1): 
* 
*  Simples implementação da primeira equação
*  Input:
*     n - Primeiro input dado pelo utilziador ("a" do enunciado) 
*  
*  Output:
*     Tupla onde o primeiro elemento é o resultado da função e o segundo é
*     o número de vezes que a função sn1 foi chamada (incluindo na funcao sum).
*)
let rec sn1 = function
  | 0 -> 1, 1 
  | 1 -> 2, 1 
  | n ->
    let (a, count), (sumResult, sumCount) = sn1 (n - 1), sum n 1 sn1 in
      3 * a + sumResult, count + sumCount + 1


(**Função sn2 (equação 2): 
* 
*  Simples implementação da segunda equação
*  Input:
*     n - Primeiro input dado pelo utilziador ("a" do enunciado) 
*  
*  Output:
*     Tupla onde o primeiro elemento é o resultado do sumatório e o segundo é
*     o número de vezes que a função sn2 foi chamada.
*)
let rec sn2 = function
  | 0 -> 1, 1
  | 1 -> 2, 1
  | n ->
    let (a, count1), (b, count2) = sn2 (n - 1), sn2 (n - 2) in 
      (((6 * n - 3) * a) - ((n - 2) * b)) / (n + 1), count1 + count2 + 1 


(**Função sn2mem (2a parte com memoização usando a segunda equação): 
* 
*  Implementação da segunda parte do problema com memoziação.
*  Ao chegar a um valor, esse valor é inserido na hashtable, que pode depois ser usado
*   para fazer outras contas mais rapidamente, sem aumentar o call stack.
*  As contas feitas em sn2mem (n - 1) serão entao usadas para computar sn2mem (n - 2),
*   tornando este metodo muito mais eficiente comparado à função anterior.
*  Foram usadas hashtables são mais eficientes que arrays/listas, visto que permitem
*   uma pesquisa extremamente eficiente. 
*
*  Input:
*     n - Segundo input dado pelo utilziador ("b" do enunciado) 
*  
*  Output:
*     Resultado (Z.t)
*
*  A função comentada abaixo é implementação da 2a parte com options
*)
let rec sn2mem n =
  match Hashtbl.mem table n with    (**check para saber se a posição n existe na hashtable. Se ja existir, é so devolver esse valor*)
  | true -> Hashtbl.find table n
  | false -> begin
    let a, b = sn2mem (n - 1), sn2mem (n - 2) in
    let nZ = zi n in
    let res = (a *! (six *! nZ -! three) /! (nZ +! one)) -! (b *! (nZ -! two) /! (nZ +! one)) in
      Hashtbl.add table n res;      (**após calcularmos o valor, inserimos-lo na hashtable*)
      res                           (**e damos return*)
  end

(*let rec sn2memOpt n =
  match Hashtbl.find_opt table n with
  | Some res -> res
  | None -> begin
    let a, b = Option.get (sn2mem (n - 1)), Option.get (sn2mem (n - 2)) in
    let nZ = zi n in
    let res = (a *! (six *! nZ -! three) /! (nZ +! one)) -! (b *! (nZ -! two) /! (nZ +! one)) in
      Hashtbl.add table n (Some res);
      Some res
  end*)


(*let sn2test n =
  let rec sn22 n k =
    match n with
    | 0 -> k one
    | 1 -> k two
    | n -> 
      let nZ = zi n in
      sn22 (n - 1) (fun a -> sn22 (n - 2) (fun b -> k ((((six *! nZ -! three) *! a) -! ((nZ -! two) *! b)) /! (nZ +! one))))
  in
  sn22 n (fun a -> a)*)

(**Chamadas da primeira parte*)
let print_tuple (x, y) = Printf.printf "%d %d\n" x y 
let () = List.hd args |> sn1 |> print_tuple
let () = List.hd args |> sn2 |> print_tuple

(**Chamadas da segunda parte*)
let () = Printf.printf "%s\n" (List.nth args 1 |> sn2mem |> Z.to_string)
(*let () = Printf.printf "%s\n" (List.nth args 1 |> sn2test |> Z.to_string)*)
(**options (ligeiramente mais lenta)*)
(*let () = Printf.printf "%s\n" (List.nth args 1 |> sn2memOpt |> Option.get |> Z.to_string)*)

let timeEnd = Sys.time ()
let () = Printf.printf "%.40f\n" (timeEnd -. timeStart)

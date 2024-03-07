let rec maiorLucro  = function 
  | [] -> raise (Invalid_argument "Lista vazia")
  | [precoTotal] -> precoTotal
  | precoTotal :: precos -> max precoTotal (maiorLucro precos)


let rec desceTabela (posicaoAtual : int) (tabela : (int , int) Hashtbl.t) (menorKey : int)=
  if (posicaoAtual = menorKey) then posicaoAtual
  else
    if(Hashtbl.mem tabela (posicaoAtual -1) ) then 
      (posicaoAtual -1 )
    else
      desceTabela (posicaoAtual-1) tabela menorKey

let rec listaEAUltima  numMenor tamDaUltima = (**Devolve se é a última lista possivél*)
  if tamDaUltima = 0 then []
  else
    numMenor :: listaEAUltima numMenor (tamDaUltima-1) 

let rec valorDaLista = 
  function
  | [] -> 0
  | e :: lista -> e +valorDaLista lista

let rec precoDaLista tabela =
  function
  | [] -> 0
  | e :: lista -> Hashtbl.find tabela e +precoDaLista tabela lista

let rec todosIguais = function (**Estudar a função*)
  | [] -> true
  | e::lista -> List.for_all ((=) e) lista && todosIguais lista

let rec contains_list lsts lst =
  List.exists (fun l -> l = lst) lsts ||
  match lsts with
  | [] -> false
  | hd::tl -> contains_list tl lst
let contains_other_elements elem1 elem2 lst =
  let rec check_elements lst =
    match lst with
    | [] -> false                       (* empty list, no other elements *)
    | x :: xs ->
      if x <> elem1 && x <> elem2 then true  (* element doesn't match either elem1 or elem2 *)
      else check_elements xs               (* recursively check the rest of the list *)
  in check_elements lst
  
let quantosDiferentes numDeDiferentes elem1 elem2 lst =
  let rec check_elements numDeDiferentes lst =
    match lst with
    | [] -> numDeDiferentes                       (* empty list, no other elements *)
    | x :: xs ->
      if x <> elem1 && x <> elem2 then check_elements (numDeDiferentes+1) xs  (* element doesn't match either elem1 or elem2 *)
      else check_elements numDeDiferentes xs               (* recursively check the rest of the list *)
  in check_elements numDeDiferentes lst 

let rec returnTheNumber numAEntrar =  (*not necessary*)
  function
  | [] -> 0
  | hd :: tl -> if(hd = numAEntrar) then hd
  else returnTheNumber numAEntrar tl 
let rec reduceTheLast numDePartida numMenor = function
| [] -> []
| hd :: tl -> if(hd <> numDePartida && hd <> numMenor)then 
tl
else 
hd :: reduceTheLast numDePartida numMenor tl

let changeTheList numDePartida numAEntrar numMenor tabela lst =
List.rev (reduceTheLast numDePartida numMenor (List.rev lst)) @ [desceTabela numAEntrar tabela numMenor]
  


  
let rec lancarBolos listaDoRepetido original multiplo tabela numPartida numMenor tamDoBolo =(**Talvez incrementar o multiplo por um e não por 3*)
  if(List.mem original listaDoRepetido) then
    if valorDaLista listaDoRepetido + numPartida = tamDoBolo then 
      listaDoRepetido @ [numPartida]
    else
      if valorDaLista listaDoRepetido + numPartida > tamDoBolo  then 
        if  numPartida = numMenor then 
          lancarBolos (List.tl(List.filter (fun x -> x = original) listaDoRepetido)) (**Posso ter que ivnverter duas vezes*)
    original (multiplo + 1) tabela numPartida numMenor tamDoBolo
        else
          lancarBolos listaDoRepetido original multiplo tabela  
          (desceTabela numPartida tabela numMenor) numMenor tamDoBolo
      else
        lancarBolos (listaDoRepetido @ [numPartida]) original multiplo tabela numPartida numMenor tamDoBolo
  else
    [] 

(**Se o tamanho for impar cada lista tem que ter um numero impar de numeros impar*)
let rec main tamDoBolo tabela numDePartida numAEntrar numMenor listaDeResultados listaDeListas =
  if (numDePartida = numMenor && tamDoBolo mod numMenor = 0 &&
    (contains_list (listaDeListas) (listaEAUltima numMenor (tamDoBolo/numMenor))) ) then
      []
  else
    if (numDePartida= numMenor && tamDoBolo mod numMenor <> 0) then
      []
    else
      if (valorDaLista listaDeResultados = tamDoBolo && not (contains_list listaDeListas listaDeResultados)) then (
          (**Será que preciso de parenteses retos*) 
          let novoValor = precoDaLista tabela listaDeResultados in
        if(numAEntrar = numMenor) then
          (main tamDoBolo tabela (desceTabela numDePartida tabela numMenor) (desceTabela numDePartida tabela numMenor) numMenor [] (listaDeListas @ [listaDeResultados])) @ [novoValor]
        else 
          (main tamDoBolo tabela numDePartida (desceTabela numAEntrar tabela numMenor) numMenor [numDePartida] (listaDeListas @ [listaDeResultados])) @ [novoValor] 
      )
      else
        if valorDaLista listaDeResultados = tamDoBolo && (contains_list listaDeListas listaDeResultados) && listaDeResultados <> [] then
          if(todosIguais listaDeResultados) then 
            if ((lancarBolos (List.tl listaDeResultados) numDePartida numDePartida tabela numDePartida numMenor tamDoBolo) <> []) then
              ((main tamDoBolo tabela numDePartida numAEntrar numMenor [] listaDeListas) @ 
              [precoDaLista tabela (lancarBolos (List.tl listaDeResultados)
              numDePartida numDePartida tabela numDePartida numMenor tamDoBolo)])(**Fazer memoização*)
            else
              if(desceTabela numDePartida tabela numMenor = numDePartida) then []
              else
                main tamDoBolo tabela (desceTabela numDePartida tabela numMenor) numAEntrar numMenor [] listaDeListas 
          else
            lancarBolos (List.filter (fun x -> x = numDePartida) listaDeResultados) numDePartida numDePartida tabela numDePartida numMenor tamDoBolo
        else
          if valorDaLista listaDeResultados > tamDoBolo  then
          (
            if( numAEntrar <> numMenor) then
              main tamDoBolo tabela numDePartida (desceTabela numAEntrar tabela numMenor) numMenor (List.rev (List.tl (List.rev  (listaDeResultados : int list))))  listaDeListas
            else
              if (not(contains_other_elements numDePartida numMenor listaDeResultados)) then
            (*So se faz quando todos os elementos da lista são o numMenor menos o numDePartida*) 
              main tamDoBolo tabela (desceTabela numDePartida tabela numMenor) (desceTabela numDePartida tabela numMenor)
                numMenor [] listaDeListas
              else
                (**se contem um elemento diferente*)                            
                if(quantosDiferentes 0 numDePartida numMenor listaDeResultados = 1) then 
                  main 9 tabela numDePartida (desceTabela numAEntrar tabela numMenor) numMenor [numDePartida] listaDeListas
                else
                  main 9 tabela numDePartida numAEntrar numMenor (changeTheList numDePartida numAEntrar numMenor tabela listaDeResultados) listaDeListas                                                            (**Afinal isto não implica necessariamente que numP = numM*)
          )
          else
            main tamDoBolo tabela numDePartida numAEntrar numMenor (listaDeResultados @ [numAEntrar]) listaDeListas @ [precoDaLista tabela listaDeResultados]
          
              
let fatias = read_int() (**>0 <=10000 int Inv exc try with*)
let numDePrecos = read_int() (**o mesmo que o de cima mas menor ou igual que fatias*)
let tabelaDosPrecos = Hashtbl.create numDePrecos 
let maiorKey = ref 0
let menorKey = ref 10000 
let ()= for i= 1 to numDePrecos do
  let keyAtual, value = Scanf.scanf " %i %i" (fun x y -> x,y) in
  Hashtbl.add tabelaDosPrecos keyAtual (value);
  if(!maiorKey < keyAtual) then maiorKey := keyAtual else ();
  if(!menorKey > keyAtual) then menorKey := keyAtual else ()
 done;

let listaDeLucros = main fatias tabelaDosPrecos !maiorKey !maiorKey !menorKey [] [[]] in
let resultado = maiorLucro listaDeLucros in (**Este é o preço minímo *)
print_int(resultado)

(**Manuel Garcia 45500, Joel Tapia 47275*)
(**Demos uso a:
    Chat GPT - https://chat.openai.com/
    OCaml Org - https://ocaml.org/
    Real World Ocaml , Yaron Minsky, Anil Madhavapeddy, Jason Hickey
    Apprendre à programmer avec OCaml ,Conchon Sylvain traduzido pelo regente da cadeira muito bem
    Michael Ryan Clarkson course , https://www.youtube.com/watch?v=MUcka_SvhLw&list=PLre5AT9JnKShBOPeuiD9b-I4XROIJhkIU
    E gostariamos também de agradecer aos docentes da cadeira
    *)

(**Definição de duas structs uma imahgem que pode ser uma folha da struct color e um nodo com 4 imagens folhas ou nodos *)
type color = W | B (** W: White , B : Black *)
type image = L of color(*leaf of one color*)
            | N of image*image*image*image (** node with four children*)
(*Usar o roda 90 duas vezes limpar o codigo e comentar*)


(*Nodo falso Folha true*)
let folhaOuNodo  (x : int) (x' : int) (y : int) (y' : int) (inicial : char) (arrBinario : char array array) : bool =
  let x1 = ref x in 
  let y1 = ref y in
  let is_folha = ref true in
  while !x1 <= x' && !is_folha do
    while !y1 <= y' && !is_folha do
      if inicial <> arrBinario.(!x1).(!y1) then is_folha := false;
    incr y1
    done;
    incr x1;
    y1 := y ;
  done;
  !is_folha

(**constrói a árvore a partir do array bidimensional recursivamente dá uso a função folhaOuNodo para saber como criar a arvore *)
let rec returnaFolhaOuNodo (partida : (int * int)) (ladoOuFim : (int*int)) (comprimento: int) (arrBinario : char array array) : image =
  let (x,y) = partida in
  let (x',y') = ladoOuFim in
  let inicial =  arrBinario.(x).(y) in

  let folha = folhaOuNodo   x x' y y' inicial arrBinario(*Fazer função aqui de booleano*)
  
in if (folha) then (if inicial = '0' then L(W) else L(B)) else (N(
  
(returnaFolhaOuNodo (x+ (comprimento/2)  , y)                    (x' , y' - (comprimento /2 ) ) (comprimento/2) arrBinario ) ,
(returnaFolhaOuNodo (x,y)                                 (x' - (comprimento / 2) ,y' - (comprimento / 2) ) (comprimento/2) arrBinario) ,
(returnaFolhaOuNodo (x, y +(comprimento /2) )                    (x' - (comprimento/2) , y') (comprimento/2) arrBinario) ,
(returnaFolhaOuNodo (x + (comprimento/2) , y + (comprimento /2) ) (x',y') (comprimento/2) arrBinario ) ))
    

(**Funções fáceis de implementar devolve o número de folhas e de nodos *)     
let rec contaFolhas (t : image) =
  match t with 
  | L(_) -> 1
  | N(primeiro,segundo,terceiro,quarto) -> 0 + contaFolhas primeiro +
  contaFolhas segundo + contaFolhas terceiro + contaFolhas quarto 
  

let rec contaNodos (t: image) =
  match t with
  | L(_) -> 0
  | N(primeiro,segundo,terceiro,quarto) -> 1 + contaNodos primeiro +
  contaNodos segundo + contaNodos terceiro + contaNodos quarto
  
(**Funções que calculam a altura minima e maxima das folhas *)  

let rec alturaDoRamoMinima (t : image) (altura : int)=
match t with
  | L(_) -> altura
  | N(primeiro,segundo,terceiro,quarto) -> min  (min (alturaDoRamoMinima primeiro (altura + 1))
  (alturaDoRamoMinima segundo (altura + 1)) ) 
  ( min (alturaDoRamoMinima terceiro (altura + 1)) (alturaDoRamoMinima quarto (altura + 1)) )

let rec alturaDoRamoMaxima (t : image) (altura : int)=
match t with
  | L(_) -> altura
  | N(primeiro,segundo,terceiro,quarto) -> max  (max (alturaDoRamoMaxima primeiro (altura + 1))
  (alturaDoRamoMaxima segundo (altura + 1)) ) 
  ( max (alturaDoRamoMaxima terceiro (altura + 1)) (alturaDoRamoMaxima quarto (altura + 1)) )

(**Isto é uma função recursivamente simples depois de percebermos que um quadrado pode ser dividido em quatro quadrantes neste caso usámos os da matemática *)

  let rec rodaNoventaEsq (t:image) =(**Bom trabalho tropa XD*)
    match t with
    |L(W) -> L(W)
    |L(B) -> L(B)
    |N(p,s,te,q) -> N(s|> rodaNoventaEsq, te |> rodaNoventaEsq,  q |> rodaNoventaEsq,   p|> rodaNoventaEsq)

(**esta função como a de cima recebe a quad tree e troca as folhas pretas pelas brancas *)

  let rec trocaBinaria (t:image) = 
    match t with 
    
    |L(W) -> L(B)
    |L(B) -> L(W)
    |N(p,s,te,q) -> N(p |> trocaBinaria, s |> trocaBinaria , te |> trocaBinaria , q |> trocaBinaria)

(**Esta função foi utilizada mas faz o mesmo que a rodaNoventaEsq mas 2 vezes usa-la faz o código mais fácil de entender*)

  let rec roda180Dir (t: image) =
    match t with 
    |N(p,s,te,q) -> N(te |> roda180Dir,q  |> roda180Dir,p |> roda180Dir,s |> roda180Dir)
    |L(W) -> L(W)
    |L(B) -> L(B)

(**Esta função foi feita para guardar a árvore no arrBinario mais uma vez a compreenssão das dimensões do quadrado e dos quadrantes têm grande importância :) *)

let rec aBsave (t : image) (arrBinario: char array array) (largura : int) (parDeEntrada : (int*int)) (parDeSaida : (int* int)) : unit =
  let (x,y) = parDeEntrada in let (x',y') = parDeSaida in
  if((*Hashtbl.length tabelaBinariaNova = 1*) Array.length arrBinario = 1) then (*Hashtbl.add tabelaBinariaNova parDeEntrada*)
  arrBinario.(x).(y) <-   (match t with L(W) -> '0' | L(B) ->'1' | _->'3')
  else
    (
      match t with 
      L(W) ->  for i = x to x' do for j = y to y' do
         arrBinario.(i).(j) <- '0'
       done done
      |L(B) ->  for i = x to x' do for j = y to y' do
        arrBinario.(i).(j) <- '1' done done
      |N(primeiro,segundo,terceiro,quarto) ->  
       let () =aBsave  (primeiro) arrBinario (largura/2) (x + largura/2 , y) (x' , y'- largura/2 ) in
       let () =aBsave  (segundo) arrBinario (largura/2) (x , y) (x' -largura/2 , y'- largura/2 ) in
       let () =aBsave  (terceiro) arrBinario (largura/2) (x , y + largura/2 ) (x' -largura/2 , y') in
       aBsave  (quarto) arrBinario (largura/2) (x + largura/2  , y + largura/2 ) (x'  , y') 


    )

  (**Esta função recebe uma string e corta-a em pedaços segundo o espaço(' ') como delimitador so pode apanjhar numeros serve para receber o lado do quadrado *)
  let split (delimiter : char) (str : string) : int array =
    let hop = String.index str ' ' in
    let str_len = String.length str in
    let arr_len = str_len/2 + 1 in
    let arr = Array.make arr_len 2 in
    let idx = ref 0 in
    let sub_start = ref 0 in
    for i = 0 to str_len - 1 do
      if str.[i] = delimiter then (
        let substr = String.sub str !sub_start hop in
      let () =
        arr.(!idx) <- int_of_string ( substr) in
        let () =
        idx := !idx + 1 in
        sub_start := !sub_start + hop +1 
      )
    done;
    let last_substr = String.sub str !sub_start (hop) in
    arr.(!idx) <- int_of_string (  last_substr);
    Array.sub arr 0 arr_len
  
  
(**Esta função recebe uma string de espaços zeros e um e cria um array de zeros e uns conservando a ordem nesta como na de cima tivemos ajuda *)
    let splitChar (delimiter : char) (str : string) : char array =
      let str_len = String.length str in
      if str_len = 1 then let last_substr = String.sub str 0 1 in
      let arr = Array.make 1 '2' in
      arr.(0) <-  (  last_substr.[0]);
      arr
      else
      let hop = String.index str ' ' in
      
      let arr_len = str_len/2 + 1 in
      let arr = Array.make arr_len '2' in
      let idx = ref 0 in
      let sub_start = ref 0 in
      for i = 0 to str_len - 1 do
        if str.[i] = delimiter then (
          let substr = String.sub str !sub_start hop in
        let () =
          arr.(!idx) <-  ( substr.[0]) in
          let () =
          idx := !idx + 1 in
          sub_start := !sub_start + hop +1 
        )
      done;
      let last_substr = String.sub str !sub_start (hop) in
      arr.(!idx) <-  (  last_substr.[0]);
      Array.sub arr 0 arr_len
    
(*A partir daqui é a leitura de uma matriz no modo P1*)
let linhaDosTamanhos = read_line()
let arrDosTamanhos = Array.copy  ((split ' ' linhaDosTamanhos)) 

let largura = arrDosTamanhos.(0)
let altura = arrDosTamanhos.(1)



let arrBinario = Array.make_matrix (largura ) (altura) '2' 

let () =
for i = 0 to altura -1  do
  let linha = read_line() in
  
  
  let arrDaLinha = Array.copy ((splitChar ' ' linha)) in
  
  for j = 0 to largura - 1 do 
   
    
    
    arrBinario.(i).(j) <- arrDaLinha.(j)
    done
done ;;
(*Este número é sempre uma potencia de 4 superior a 1*)



let tree =returnaFolhaOuNodo (0, 0) (largura -1 , altura -1) (largura) (arrBinario)  in
let () =
Printf.printf"%d %d\n" ( contaFolhas ( tree ))  
(contaNodos (tree))  in
let () =
Printf.printf"%d %d\n" (alturaDoRamoMinima ( tree ) 0)
(alturaDoRamoMaxima ( tree ) 0)
in

let ninetyTree = (tree) |> rodaNoventaEsq  in



aBsave ninetyTree arrBinario largura (0,0) (largura - 1,  altura - 1);



print_string(mode);
print_newline();
print_string(linhaDosTamanhos);
print_newline();

let() =
for i = 0 to altura-1 do
  for j =0 to largura-1 do
    
    print_char(arrBinario.(i).(j) );
    if(j != largura - 1 ) then
      print_char(' ')
    else(print_newline();) 
  done
done in

let arbolInvertida = trocaBinaria (ninetyTree) in

aBsave arbolInvertida arrBinario (largura ) (0,0) (largura- 1, altura - 1);

print_string(mode);
print_newline();
print_string(linhaDosTamanhos);
print_newline();

let() =
for i = 0 to altura -1 do
  for j = 0 to largura -1  do
    
    print_char(arrBinario.(i).(j) );
    if(j != largura - 1 ) then
      print_char(' ')
    else(print_newline();)  
  done
done in


aBsave ((roda180Dir arbolInvertida)  ) arrBinario (largura ) (0,0) (largura - 1, altura - 1);

print_string(mode);
print_newline();
print_string(linhaDosTamanhos);
print_newline();

for i = 0  to altura - 1 do
  for j = 0 to largura - 1 do
    
    print_char(arrBinario.(i).(j) );
    if(j != largura - 1 ) then
      print_char(' ')
    else(print_newline();) 
  done
done
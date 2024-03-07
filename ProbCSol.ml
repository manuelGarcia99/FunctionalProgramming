type color = W | B (** W: White , B : Black *)
type image = L of color(*leaf of one color*)
            | N of image*image*image*image (** node with four children*)



let rec returnaFolhaOuNodo (partida : (int * int)) (ladoOuFim : (int*int)) comprimento tabelaBinaria =
  let inicial = Hashtbl.find tabelaBinaria partida in
  let (x,y) = partida in
  let (x',y') = ladoOuFim in
  let counter = ref 0 in
  let () =
  for i = x to x' do
    for j = y to y' do 
      if inicial <> Hashtbl.find tabelaBinaria (i,j) then 
      incr counter 
      else
        ()
    done
  done
in if (!counter > 0) then (
  
  
  N(
  (returnaFolhaOuNodo (x,y) (x' - (comprimento / 2),y' - (comprimento / 2)) (comprimento/2) tabelaBinaria) ,
  (returnaFolhaOuNodo (x+ (comprimento/2) , y) (x' , y' - (comprimento /2 ) ) (comprimento/2) tabelaBinaria ) ,
  (returnaFolhaOuNodo (x + (comprimento/2)  , y + (comprimento /2)) (x',y') (comprimento/2) tabelaBinaria ) ,
  (returnaFolhaOuNodo (x, y +(comprimento /2)) (x' - (comprimento/2) , y') (comprimento/2) tabelaBinaria) 
  
)) else (if inicial = 0 then L(W) else L(B))
    
     
let rec contaFolhas (t : image) =
  match t with 
  | N(primeiro,segundo,terceiro,quarto) -> 0 + contaFolhas primeiro +
  contaFolhas segundo + contaFolhas terceiro + contaFolhas quarto 
  | L(_) -> 1

let rec contaNodos (t: image) =
  match t with
  | N(primeiro,segundo,terceiro,quarto) -> 1 + contaNodos primeiro +
  contaNodos segundo + contaNodos terceiro + contaNodos quarto
  | L(_) -> 0

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

let rec rodaNoventa (t:image) =
  match t with
  |L(W) -> L(W)
  |L(B) -> L(B)
  |N(p,s,t,q) -> N(rodaNoventa q , rodaNoventa p, rodaNoventa s, rodaNoventa t)

let rec devolveElemento (lista : int list) (numIndexNoZero : int)  =
  match lista with 
  |[] -> failwith "Lista vazia"
  |head :: tail -> if(numIndexNoZero = 1) then head else devolveElemento tail (numIndexNoZero - 1)

let split delimiter str =
  let rec split_helper acc str =
    try
      let idx = String.index str delimiter in(*Posso usar este modulo??*)
      let s = String.sub str 0 idx in
      let rest = String.sub str (idx + 1) (String.length str - idx - 1) in
      split_helper (s :: acc) rest
    with Not_found ->
      str :: acc
  in
  List.rev (split_helper [] str)

let rec mostraLista lista  = 
  match lista with 
  |[] -> print_newline;
  | hd :: tail -> print_int(hd); print_newline(); mostraLista tail ;;




let mode = read_line()
let linhaDosTamanhos = read_line()
let listaDosTamanhos = List.map (fun x -> int_of_string x) ((split ' ' linhaDosTamanhos))

let largura = devolveElemento listaDosTamanhos 1
let altura = devolveElemento listaDosTamanhos 2


let tabelaBinaria = Hashtbl.create (largura * altura ) 




let () =
for i = 1 to altura do
  let linha = read_line() in

  
  let listaDaLinha = List.map (fun x -> int_of_string x) ((split ' ' linha)) in
  
  for j = 1 to largura do 
   
    
    Hashtbl.add tabelaBinaria (i , j) (devolveElemento listaDaLinha j)
    done
done 
(*Este número é sempre uma potencia de 4 superior a 1*)

let numDeParticoes = read_int() 

let () =
Printf.printf"%d %d\n" ( contaFolhas ( returnaFolhaOuNodo (1, 1) (largura , altura) (largura) (tabelaBinaria) ))  
(contaNodos (returnaFolhaOuNodo (1, 1) (largura , altura) (largura) (tabelaBinaria))) 
let () =
Printf.printf"%d %d\n" (alturaDoRamoMinima ( returnaFolhaOuNodo (1, 1) (largura , altura) (largura) (tabelaBinaria) ) 0)
(alturaDoRamoMaxima ( returnaFolhaOuNodo (1, 1) (largura , altura) (largura) (tabelaBinaria) ) 0)

(*let arrayBinario = Array.make (largura * altura) (2)*) 

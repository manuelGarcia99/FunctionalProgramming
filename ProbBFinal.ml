let rec melhorFatia tamDoBolo tabela tabelaDosUsados listaDosDemasiadoGrandes numMaior numMenor keyMelhorRel max posicaoAtual= (**Melhor relação começa a 0 *)
if (posicaoAtual = numMenor) then if((float_of_int tamDoBolo /. float_of_int numMenor) *. float_of_int (Hashtbl.find numMenor) = max) then 
  numMenor
else
  if((float_of_int tamDoBolo /. float_of_int numMenor) *. float_of_int (Hashtbl.find numMenor) < max)then 
    keyMelhorRel
  else
















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



 (**Criar uma função que devolva o maior lucro por fatia *)
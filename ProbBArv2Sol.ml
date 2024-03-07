
let rec valorDaLista = 
  function
  | [] -> 0
  | e :: lista -> e +valorDaLista lista

  let rec maiorLucro = function 
   | [] -> raise (Invalid_argument "Lista vazia 2")
   | [precoTotal] -> precoTotal
   | precoTotal :: precos -> max precoTotal (maiorLucro precos)

  let rec desceTabela  (tabela : (int , int) Hashtbl.t) (posicaoAtual : int) (menorKey : int)=
   if (posicaoAtual = menorKey) then posicaoAtual
   else
     if(Hashtbl.mem tabela (posicaoAtual -1) ) then 
       (posicaoAtual -1 )
     else
       desceTabela  tabela (posicaoAtual-1) menorKey

  let rec listaDosPequenos tabela tamDoBolo lista menorKey  =
   if(tamDoBolo - menorKey < 0 ) then
     lista
   else
     if(tamDoBolo - menorKey = 0) then 
       lista @ [Hashtbl.find tabela menorKey]
 else
   listaDosPequenos tabela (tamDoBolo - menorKey) (lista @ [Hashtbl.find tabela menorKey]) menorKey 
 
 let rec listaDosGrandes tabela tamDoBolo lista keyAtual menorKey =
   if(tamDoBolo - menorKey <0 ) then 
     lista
 else
   if(tamDoBolo- menorKey =0 ) then 
     lista @ [Hashtbl.find tabela menorKey]
 else
   if(keyAtual <> menorKey)then 
     if(tamDoBolo - keyAtual <0 ) then
       listaDosGrandes tabela tamDoBolo lista (desceTabela  tabela keyAtual menorKey) menorKey
     else
       if(tamDoBolo - keyAtual =0 ) then 
         lista @ [Hashtbl.find tabela keyAtual]
       else
         listaDosGrandes tabela (tamDoBolo-keyAtual) (lista @ [Hashtbl.find tabela keyAtual]) keyAtual menorKey
   else
     listaDosGrandes tabela (tamDoBolo - menorKey) (lista @ [Hashtbl.find tabela menorKey]) menorKey menorKey

     (**Pode não ter que se usar o numMenor(*1*) mas não pode haver erros no resto da
        implementação do código porque não podem haver números maiores que zero
        nos indćes que não constam entre as keys da tabela*)

    let precoDoArray tabelaDosPrecos  arrNumDeFatiasDeTamanho  (*1*)=
      let sum = ref 0 in 
      let() =
      for i = 1 to Array.length arrNumDeFatiasDeTamanho -1  do
        sum := !sum + (if( arrNumDeFatiasDeTamanho.(i) <> 0) then
           (arrNumDeFatiasDeTamanho.(i) * (Hashtbl.find tabelaDosPrecos (i) )) else 0)
     done in !sum
    (*Esta função devolve o valor do Array o quanto ele ocupa de todo o bolo*)
     let valorDoArray arrNumDeFatiasDeTamanho =
      
      let sum =ref 0 in
      let () =
      for i = 1 to Array.length arrNumDeFatiasDeTamanho -1 do
        sum := !sum + i* arrNumDeFatiasDeTamanho.(i) done in !sum
      (*Esta função pega em todos os preços possiveis que são primissores para
         a venda do bolo e também no preço novo armazenado no array e verifica se
         este lucro já foi armazenado se foi returna true se não foi false*)
    let contemPreco tabelaDosPrecos arrNumDeFatiasDeTamanho listaDeLucros =
      let preco = precoDoArray  tabelaDosPrecos arrNumDeFatiasDeTamanho in
      if(preco = 0 ) then false else
      let rec itera preco  listaDeLucros =
      match listaDeLucros with
      | [] -> false
      | x::xs -> if(x= preco) then true else itera preco xs
      in itera preco listaDeLucros

    (*Esta função pega no array e verifica se só existe uma célula diferente de 0
       se for passado um array vazio ou inalterado então ele devolve uma exception os indices
       correspondem aos tamanhos de fatia e os values correspondem á quantidade de fatias*)

    let arrayContemFatiasDeUmSoTamanho arrNumDeFatiasDeTamanho =
      let sum = ref 0 in
      let () =
      for i = 1 to Array.length arrNumDeFatiasDeTamanho -1 do
      if(arrNumDeFatiasDeTamanho.(i) <> 0) then incr sum
      else () done in
      if !sum =0  then raise (Invalid_argument "Array Vazio na verificação se ele só tem elementos de 
        um tipo\n")
      else if(!sum =1 ) then true else false

    (*Esta função determina a quantidade de fatias de tamanhos diferentes dos tamanhos passados
       não devolve o tamanho por fatia mas sim o número de fatias diferentes de eles os 2*)
    let quantosTamanhosDeBoloDiferentesParaAlemDoMenorOuPartida
    numADevolverDeDiferentes numDePartida numMenor arrNumDeFatiasDeTamanho =
    let sum = ref 0 in
    let () =
     for i= 1 to Array.length arrNumDeFatiasDeTamanho -1 do
      if(arrNumDeFatiasDeTamanho.(i)<>0 && i <> numDePartida && i <> numMenor)
         then sum := !sum + arrNumDeFatiasDeTamanho.(i) 
      else () done in !sum
    (*Esta função apaga uma fatia do indice do tamanho imediatamente anterior
    (em termos de ordem ascendente) ao do numDePartidas*)
    let  apagarUmDiferente numDePartida numMenor arrNumDeFatiasDeTamanho =
      let jaEsta = ref false in
      for i = Array.length arrNumDeFatiasDeTamanho -1 downto 1 do
        if((i <> numDePartida && i<> numMenor) && arrNumDeFatiasDeTamanho.(i) <> 0
          && not !jaEsta)
          then (
            let () =
            arrNumDeFatiasDeTamanho.(i) <- arrNumDeFatiasDeTamanho.(i) -1 in
            jaEsta := true
          )
        else
          () done
          (*Esta função executa a apagarUmDiferente e depois mete incrementa
              o indice do número imediatamente abaixo do indíce númeroAEntrar
              esta função poderá ser útil em mais que um sítio até na função das 
              combinações*)
let adicionaUmNumeroMaisPequenoQueONumeroAEntrarRemovendoONumeroImediatamenteMenorAoDoPartida 
tabelaDosPrecos numDePartida numAEntrar numMenor arrNumDeFatiasDeTamanho =
let () =
apagarUmDiferente numDePartida numMenor arrNumDeFatiasDeTamanho in
let() =
arrNumDeFatiasDeTamanho.(desceTabela tabelaDosPrecos numAEntrar numMenor) <- arrNumDeFatiasDeTamanho.(desceTabela tabelaDosPrecos numAEntrar numMenor) +1 in
arrNumDeFatiasDeTamanho

(*Esta função apaga todas as fatias do array que não estão no indice do Numero de Partida*)
let recriaArrayComFatiasSoNoNumeroDePartida numeroDePartida arrNumDeFatiasDeTamanho =
  let () =
  for i = 1 to Array.length arrNumDeFatiasDeTamanho -1 do 
    if(i<>numeroDePartida) then arrNumDeFatiasDeTamanho.(i)  <- 0
    else () done
  in arrNumDeFatiasDeTamanho

(*Esta função cria um array com uma fatia no indice numero de Partida*)
let arrayDoNumeroDePartida numDePartida tamanho =
  let arr =Array.make (tamanho ) 0 in let() =
  arr.(numDePartida) <- 1  in arr
(*Esta função decrementa o indice menor que esteja preenchido por fatias poderemos ter que 
   decrementar o indice do numero a entrar mas até agora não foi possivel imaginar uma situação 
   em que há numeros menores que o numero a entrar quando o numero a entrar está a ser passado para
   o array logo se vê também é suposto que a função decremente se o indice em causa for maior que 
   o numero a Entrar*)
let apagaOUltimo arrNumDeFatiasDeTamanho =
  let jaEsta = ref false in let () =
  for i = 1 to Array.length arrNumDeFatiasDeTamanho -1 do
    if(arrNumDeFatiasDeTamanho.(i) <> 0 && not !jaEsta) then
       (let () =
        arrNumDeFatiasDeTamanho.(i) <- arrNumDeFatiasDeTamanho.(i) -1 in
    jaEsta := true)
  else () done in arrNumDeFatiasDeTamanho 

  let rec sobeTabela  (tabela : (int , int) Hashtbl.t) (posicaoAtual : int) (maiorKey : int)=
   if (posicaoAtual = maiorKey) then posicaoAtual
   else
     if(Hashtbl.mem tabela (posicaoAtual +1) ) then 
       (posicaoAtual +1 )
     else
       sobeTabela tabela (posicaoAtual +1) maiorKey
    
(*1- Ainda não estamos prontos para sair da função 
  2- Neste caso o numero de partida ou não é o menor ou o número a entrar não é o menor ou ainda há espaço para expandir o bolo
  3- Vem do 2 so ocorre porque ou o numero de partida ou o de entrar são de certeza diferentes do menor
  4- Vem do 3 so ocorre porque o numero a entrar não é diferente do menor ou ainda cabem numeros a entrar no bolo
  5- Vem do 4 so ocorre quando há certezas que o num de Partida é diferente o número menor também
  6- Vem do 5 so ocorre quando são diferentes e também já sabemos que não há espaço
  *)
  
let rec combinacoes fatias tabelaDosPrecos arrNumDeFatiasDeTamanho original numDePartida numAEntrar numMenor
 minimo (listaDosLucros : int list) = 
(** let () = print_string("     ") in let () = print_int(arrNumDeFatiasDeTamanho.(original)) in*)
 if(quantosTamanhosDeBoloDiferentesParaAlemDoMenorOuPartida 0 original numMenor arrNumDeFatiasDeTamanho =0 &&
   arrNumDeFatiasDeTamanho.(original) = 1 &&valorDoArray arrNumDeFatiasDeTamanho + numMenor > fatias ) then listaDosLucros
else(*1*)if(numDePartida = numMenor && numAEntrar = numMenor && valorDoArray arrNumDeFatiasDeTamanho + numAEntrar > fatias) then (
  if(precoDoArray tabelaDosPrecos arrNumDeFatiasDeTamanho >= minimo && 
    not(contemPreco tabelaDosPrecos arrNumDeFatiasDeTamanho listaDosLucros)) then 
      combinacoes fatias tabelaDosPrecos arrNumDeFatiasDeTamanho original numDePartida numAEntrar numMenor
  minimo (listaDosLucros @ [precoDoArray tabelaDosPrecos arrNumDeFatiasDeTamanho])
  else (let () = 
  arrNumDeFatiasDeTamanho.(sobeTabela tabelaDosPrecos numAEntrar original) <-
   arrNumDeFatiasDeTamanho.(sobeTabela tabelaDosPrecos numAEntrar original ) -1 in
   let () =
   while(valorDoArray arrNumDeFatiasDeTamanho <= fatias- numAEntrar) do
   arrNumDeFatiasDeTamanho.(numAEntrar)<- arrNumDeFatiasDeTamanho.(numAEntrar) +1 done in combinacoes fatias tabelaDosPrecos arrNumDeFatiasDeTamanho original numDePartida numAEntrar numMenor
   minimo listaDosLucros)
)
else(*2*)if(numDePartida = numMenor && numAEntrar = numMenor && valorDoArray arrNumDeFatiasDeTamanho +numAEntrar <= fatias) then (
  let () =arrNumDeFatiasDeTamanho.(numAEntrar) <- arrNumDeFatiasDeTamanho.(numAEntrar) + 1 in 
  combinacoes fatias tabelaDosPrecos arrNumDeFatiasDeTamanho original numDePartida numAEntrar numMenor
  minimo listaDosLucros
)
else(*3*)if(numDePartida <> numMenor && numAEntrar = numMenor && valorDoArray arrNumDeFatiasDeTamanho + numAEntrar> fatias) then (
  
  if(precoDoArray tabelaDosPrecos arrNumDeFatiasDeTamanho >= minimo && 
    not(contemPreco tabelaDosPrecos arrNumDeFatiasDeTamanho listaDosLucros)) then (
      
      combinacoes fatias tabelaDosPrecos arrNumDeFatiasDeTamanho original numDePartida numAEntrar numMenor
  minimo (listaDosLucros @ [precoDoArray tabelaDosPrecos arrNumDeFatiasDeTamanho]))
 else(*ori*) if(arrNumDeFatiasDeTamanho.(original) > 1 && arrNumDeFatiasDeTamanho.(original) <> 1) then (
  
   let () = 
 arrNumDeFatiasDeTamanho.(sobeTabela tabelaDosPrecos numAEntrar original) <-
  arrNumDeFatiasDeTamanho.(sobeTabela tabelaDosPrecos numAEntrar original ) -1 in
  let () =
  while(valorDoArray arrNumDeFatiasDeTamanho <= fatias- numAEntrar) do
  arrNumDeFatiasDeTamanho.(numAEntrar)<- arrNumDeFatiasDeTamanho.(numAEntrar) +1 done in combinacoes fatias tabelaDosPrecos arrNumDeFatiasDeTamanho original numDePartida
   (if arrNumDeFatiasDeTamanho.(original) = 1 then (desceTabela tabelaDosPrecos numAEntrar numMenor) else numAEntrar) numMenor
  minimo listaDosLucros)
else 
  
  let () = arrNumDeFatiasDeTamanho.(numAEntrar) <- arrNumDeFatiasDeTamanho.(numAEntrar) -1 in
  let() = while (valorDoArray arrNumDeFatiasDeTamanho <= fatias -(desceTabela tabelaDosPrecos numAEntrar numMenor)) do
    arrNumDeFatiasDeTamanho.(desceTabela tabelaDosPrecos numAEntrar numMenor)<- 
    arrNumDeFatiasDeTamanho.(desceTabela tabelaDosPrecos numAEntrar numMenor) +1 done in 
    combinacoes fatias tabelaDosPrecos arrNumDeFatiasDeTamanho original numDePartida numAEntrar numMenor minimo listaDosLucros

)
else(*4*)if(numDePartida <> numMenor && numAEntrar = numMenor && valorDoArray arrNumDeFatiasDeTamanho + numAEntrar<= fatias ) then (
  
  let () =arrNumDeFatiasDeTamanho.(numAEntrar) <- arrNumDeFatiasDeTamanho.(numAEntrar) + 1 in 
  combinacoes fatias tabelaDosPrecos arrNumDeFatiasDeTamanho original numDePartida numAEntrar numMenor
  minimo listaDosLucros
)
else(*5*)if(numDePartida <> numMenor && numAEntrar <> numMenor && valorDoArray arrNumDeFatiasDeTamanho +numAEntrar<= fatias) then (
  
  let () =arrNumDeFatiasDeTamanho.(numAEntrar) <- arrNumDeFatiasDeTamanho.(numAEntrar) + 1 in 
  combinacoes fatias tabelaDosPrecos arrNumDeFatiasDeTamanho original numDePartida numAEntrar numMenor
  minimo listaDosLucros
)
else(*6*)(
  if(precoDoArray tabelaDosPrecos arrNumDeFatiasDeTamanho >= minimo && 
    not(contemPreco tabelaDosPrecos arrNumDeFatiasDeTamanho listaDosLucros)) then 
      
      combinacoes fatias tabelaDosPrecos arrNumDeFatiasDeTamanho original numDePartida numAEntrar numMenor
  minimo (listaDosLucros @ [precoDoArray tabelaDosPrecos arrNumDeFatiasDeTamanho])
  else (*ori*)if(arrNumDeFatiasDeTamanho.(original) > 1) then 
    if(quantosTamanhosDeBoloDiferentesParaAlemDoMenorOuPartida 0 original numAEntrar arrNumDeFatiasDeTamanho = 0 ) then (
      let novoNumero = desceTabela tabelaDosPrecos numAEntrar numMenor in
      let ()=
      arrNumDeFatiasDeTamanho.(numAEntrar) <- arrNumDeFatiasDeTamanho.(numAEntrar) -1 in
      let() =
      while(valorDoArray arrNumDeFatiasDeTamanho <= fatias - novoNumero) do
      arrNumDeFatiasDeTamanho.(novoNumero) <- arrNumDeFatiasDeTamanho.(novoNumero) +1  
     done in
     combinacoes fatias tabelaDosPrecos arrNumDeFatiasDeTamanho original numDePartida novoNumero numMenor minimo listaDosLucros
  )
    else(
      let () = 
      arrNumDeFatiasDeTamanho.(sobeTabela tabelaDosPrecos numAEntrar original) <-
       arrNumDeFatiasDeTamanho.(sobeTabela tabelaDosPrecos numAEntrar original ) -1 in
       let () =
       while(valorDoArray arrNumDeFatiasDeTamanho <= fatias- numAEntrar) do
       arrNumDeFatiasDeTamanho.(numAEntrar)<- arrNumDeFatiasDeTamanho.(numAEntrar) +1 done in combinacoes fatias tabelaDosPrecos arrNumDeFatiasDeTamanho original numDePartida 
       (if arrNumDeFatiasDeTamanho.(original) = 1 then (desceTabela tabelaDosPrecos numAEntrar numMenor) else numAEntrar) numMenor
       minimo listaDosLucros
    ) else 
      if(numAEntrar <> original || arrNumDeFatiasDeTamanho.(original) > 1) then
let () = arrNumDeFatiasDeTamanho.(numAEntrar) <- arrNumDeFatiasDeTamanho.(numAEntrar) -1 in
  let() = while (valorDoArray arrNumDeFatiasDeTamanho <= fatias -(desceTabela tabelaDosPrecos numAEntrar numMenor)) do
    arrNumDeFatiasDeTamanho.(desceTabela tabelaDosPrecos numAEntrar numMenor)<- 
    arrNumDeFatiasDeTamanho.(desceTabela tabelaDosPrecos numAEntrar numMenor) +1 done in 
    combinacoes fatias tabelaDosPrecos arrNumDeFatiasDeTamanho original numDePartida numAEntrar numMenor minimo listaDosLucros
  else combinacoes fatias tabelaDosPrecos arrNumDeFatiasDeTamanho original numDePartida (desceTabela tabelaDosPrecos numAEntrar numMenor) 
  numMenor minimo listaDosLucros (*Nao sei se e preciso*)
) 
      

(*
1-Acontece quando o numero de partida e diferente do menor ou quando o valor do array é menor que o fatias - numeroMenor que é o quão grande a venda
pode ser
2-este else acontece o mesmo que no else 1 e retrata o caso de não termos ainda acabado de ver todas as combinações o 
   array em questão é valioso o suficiente para ultrapassar o minimo estabelecido 
   ,não são fatias a mais
   mas o preco(lucro) do array já foi armazenado na lista de lucros *)
(*3-este else retrata todas as condições no caso dois mas se inserissemos mais um numero a entrar
   de fatias
   então isso iria superar o número de fatias que há no bolo neste else vamos inicialmente fazer
   como na versão anterior se houver bugs será aqui e teremos que ver se não será melhor chamar a 
   função combinações mais cedo e não só quando são todos iguais*)
(*4-este else retrata todas as condições do caso três mas o array contém fatias de vários tamanhos e não
   só de um na função quantos fica a questão de se será mesmo necessária e se a condicional
   que verifica igualdade com o um é mesmo útil ou só foi concebida para conveniência dos 
   programadores
  5-Verificar se isto esvazia o Array como de esperado e se comporta como o pretendido
  6-Este else ocorre quando se passa exatamente o que acontece no quatro e há números diferentes
  do número A Entrar que é igual ao menor por esta altura do campeonato e do de Partida
  7-Este else ocorre quando se passa exatamente o que acontece no 6 e há dois ou mais indices
  diferentes de zero que não são o de Partida Entrar ou Menor
  8-Este else ocorre quando se passa exatamente o que acontece no 3 e o numero a entrar é maior que 
  o numero menor
  9-Este else ocorre quando se passa exatamente o que acontece no 8 e não há números diferentes do 
  número de partida e do número menor
  10- Este else acontece quando ainda há espaço para o Array crescer isto é a soma dos produtos dos
  valores pelos seus indices não excede o numero de fatias ou então o preço do array não
  excede o minimo e também é executado apenas quando ou o numero de partida é diferente do número menor
  ou quando a soma das fatias não atinge o maximo possivel pelo menos fatias possiveis menos a mais 
  pequena fatia
  11- Este else acontece quando se realiza o mesmo que no 10 e o array a rodar é de baixo preço 
  para ser relevante
  12- Vem do else 3 e o valor do bolo ainda não é grande demais
  13- Vem do else 12 e o bolo não é grande demais mas o seu preço não é bom o suficiente
  14-Mudanças acontecem com o else 10 mas o array e grande demais aqui o numAEntrar e maior que o menor e vai ser descido
  15- no caso de o array não ser grande de mais
  16- vem do 13 mas expandir o array não é uma opção
  17- vem do quize impede arrays errados
  18- vem do onze impede arrays errados
   *)
let rec main fatias tabelaDosPrecos numDePartida numAEntrar numMenor
 minimo arrDasFatias arrNumDeFatiasDeTamanho listaDeLucros =
 (*let () = print_string("SHI") in*) 
 

  if (numDePartida = numMenor && valorDoArray arrNumDeFatiasDeTamanho > fatias - numMenor) then(
    
       listaDeLucros )

  else(*1*) if ( 
    valorDoArray arrNumDeFatiasDeTamanho <= fatias ) then( 
     if(not(contemPreco tabelaDosPrecos arrNumDeFatiasDeTamanho listaDeLucros) && precoDoArray tabelaDosPrecos arrNumDeFatiasDeTamanho >= minimo ) 
      then (   
        main fatias tabelaDosPrecos numDePartida numAEntrar numMenor minimo
          arrDasFatias arrNumDeFatiasDeTamanho
           (listaDeLucros @ [precoDoArray tabelaDosPrecos arrNumDeFatiasDeTamanho]))
      else (*2*)if(valorDoArray arrNumDeFatiasDeTamanho + numAEntrar <= fatias )then(
        let() =
        arrNumDeFatiasDeTamanho.(numAEntrar) <- (arrNumDeFatiasDeTamanho.(numAEntrar) + 1) in
        main fatias tabelaDosPrecos numDePartida numAEntrar numMenor minimo
        arrDasFatias arrNumDeFatiasDeTamanho listaDeLucros
      )
    else(*3*)if  (valorDoArray arrNumDeFatiasDeTamanho >= fatias - numMenor) then( if( numAEntrar = numMenor)(*Para tirar*) then( if arrayContemFatiasDeUmSoTamanho arrNumDeFatiasDeTamanho then (
      main fatias tabelaDosPrecos numDePartida numAEntrar numMenor minimo arrDasFatias (arrayDoNumeroDePartida numDePartida (Array.length arrNumDeFatiasDeTamanho)) 
      (listaDeLucros @ (combinacoes fatias tabelaDosPrecos arrNumDeFatiasDeTamanho 
      numDePartida numDePartida numAEntrar numMenor minimo listaDeLucros) )(*Combinações*)
    )
  else(*4*)if(quantosTamanhosDeBoloDiferentesParaAlemDoMenorOuPartida
     0 numDePartida numMenor arrNumDeFatiasDeTamanho = 0) then(
      main fatias tabelaDosPrecos (desceTabela tabelaDosPrecos numDePartida numMenor)
      (desceTabela tabelaDosPrecos numDePartida numMenor) numMenor minimo arrDasFatias
       (Array.make (Array.length arrNumDeFatiasDeTamanho) 0(*5*)) listaDeLucros 
     )else (*6*) if(quantosTamanhosDeBoloDiferentesParaAlemDoMenorOuPartida
      0 numDePartida numMenor arrNumDeFatiasDeTamanho=1) then(
        main fatias tabelaDosPrecos numDePartida (desceTabela tabelaDosPrecos numAEntrar numMenor) numMenor minimo arrDasFatias 
        (arrayDoNumeroDePartida numDePartida (Array.length arrNumDeFatiasDeTamanho )) listaDeLucros
      ) else (*7*)
         main fatias tabelaDosPrecos numDePartida numAEntrar numMenor minimo arrDasFatias
         (adicionaUmNumeroMaisPequenoQueONumeroAEntrarRemovendoONumeroImediatamenteMenorAoDoPartida
         tabelaDosPrecos numDePartida numAEntrar numMenor arrNumDeFatiasDeTamanho) listaDeLucros
       ) else (*8*)if(quantosTamanhosDeBoloDiferentesParaAlemDoMenorOuPartida
        0 numDePartida numMenor arrNumDeFatiasDeTamanho >= 1) then (
          main fatias tabelaDosPrecos
           numDePartida (desceTabela tabelaDosPrecos numAEntrar numMenor) numMenor minimo
           arrDasFatias (recriaArrayComFatiasSoNoNumeroDePartida numDePartida arrNumDeFatiasDeTamanho)
           listaDeLucros 
           ) else (*9*) main fatias tabelaDosPrecos
           numDePartida (desceTabela tabelaDosPrecos numAEntrar numMenor) numMenor minimo
           arrDasFatias  (apagaOUltimo arrNumDeFatiasDeTamanho) listaDeLucros 
           )else (*12*)
           if(precoDoArray tabelaDosPrecos arrNumDeFatiasDeTamanho >= minimo
              && numAEntrar + valorDoArray arrNumDeFatiasDeTamanho < fatias) then (let() =
            arrNumDeFatiasDeTamanho.(numAEntrar) <- arrNumDeFatiasDeTamanho.(numAEntrar) + 1 in
            main fatias tabelaDosPrecos numDePartida numAEntrar numMenor minimo
            arrDasFatias arrNumDeFatiasDeTamanho
            (listaDeLucros @ [precoDoArray tabelaDosPrecos arrNumDeFatiasDeTamanho]))
           else(*13*) if(numAEntrar + valorDoArray arrNumDeFatiasDeTamanho > fatias) then
                        main fatias tabelaDosPrecos (if(numAEntrar= numMenor) then (desceTabela tabelaDosPrecos numDePartida numMenor) else(*ternario*) numDePartida ) (if(numAEntrar = numMenor) then (desceTabela tabelaDosPrecos numDePartida numMenor) else(*ternario*) (desceTabela tabelaDosPrecos numAEntrar numMenor) )numMenor minimo arrDasFatias arrNumDeFatiasDeTamanho listaDeLucros
                      else(*16*)
                        let() =
            arrNumDeFatiasDeTamanho.(numAEntrar) <- arrNumDeFatiasDeTamanho.(numAEntrar) + 1 in
            main fatias tabelaDosPrecos numDePartida numAEntrar numMenor minimo
            arrDasFatias arrNumDeFatiasDeTamanho
            listaDeLucros 

           )
           else (*10*) if(valorDoArray arrNumDeFatiasDeTamanho > fatias) then (if (numAEntrar= numMenor) then (main fatias tabelaDosPrecos (desceTabela tabelaDosPrecos numDePartida numMenor) (desceTabela tabelaDosPrecos numDePartida numMenor) numMenor minimo arrDasFatias arrNumDeFatiasDeTamanho listaDeLucros) 
           else (*14*)
            main fatias tabelaDosPrecos numDePartida (desceTabela tabelaDosPrecos numAEntrar numMenor) numMenor minimo arrDasFatias arrNumDeFatiasDeTamanho listaDeLucros)
           else (*15*)
            if(precoDoArray tabelaDosPrecos arrNumDeFatiasDeTamanho >= minimo ) then (
              if(numAEntrar + valorDoArray arrNumDeFatiasDeTamanho <= fatias) then(
            let() =
            arrNumDeFatiasDeTamanho.(numAEntrar) <- arrNumDeFatiasDeTamanho.(numAEntrar) + 1 in
            main fatias tabelaDosPrecos numDePartida numAEntrar numMenor minimo
            arrDasFatias arrNumDeFatiasDeTamanho
            listaDeLucros @ [precoDoArray tabelaDosPrecos arrNumDeFatiasDeTamanho]
               ) else(*17*) main fatias tabelaDosPrecos (if(numAEntrar= numMenor) then (desceTabela tabelaDosPrecos numDePartida numMenor) else(*ternario*) numDePartida ) (if(numAEntrar = numMenor) then (desceTabela tabelaDosPrecos numDePartida numMenor) else(*ternario*) (desceTabela tabelaDosPrecos numAEntrar numMenor) )numMenor minimo arrDasFatias arrNumDeFatiasDeTamanho listaDeLucros
           ) else (*11*)
           (
            if(numAEntrar + valorDoArray arrNumDeFatiasDeTamanho <= fatias) then
            let () =
            
            arrNumDeFatiasDeTamanho.(numAEntrar) <- arrNumDeFatiasDeTamanho.(numAEntrar) + 1 in
            main fatias tabelaDosPrecos numDePartida numAEntrar numMenor minimo
            arrDasFatias arrNumDeFatiasDeTamanho listaDeLucros
            else(*18*) main fatias tabelaDosPrecos (if(numAEntrar= numMenor) then (desceTabela tabelaDosPrecos numDePartida numMenor) else(*ternario*) numDePartida ) (if(numAEntrar = numMenor) then (desceTabela tabelaDosPrecos numDePartida numMenor) else(*ternario*) (desceTabela tabelaDosPrecos numAEntrar numMenor) )numMenor minimo arrDasFatias arrNumDeFatiasDeTamanho listaDeLucros
           )
           

       
let fatias = read_int() (**>0 <=10000 int Inv exc try with*)
let numDePrecos = read_int() (**o mesmo que o de cima mas menor ou igual que fatias*)
let tabelaDosPrecos = Hashtbl.create numDePrecos 
let maiorKey = ref 0
let menorKey = ref 10000 

let ()= for i= 1 to numDePrecos  do
  let keyAtual, value = Scanf.scanf " %i %i" (fun x y -> x,y) in
  if(keyAtual > 0 && keyAtual <= 10000) then(
  Hashtbl.add tabelaDosPrecos keyAtual (value) 
  )
   else () ;
  if(!maiorKey < keyAtual) then maiorKey := keyAtual else ();
  if(!menorKey > keyAtual) then menorKey := keyAtual else ()
 done;
 
 let arrNumDeFatiasDeTamanho = Array.make (!maiorKey +1) 0 in
 
 let minimo = max (valorDaLista ( listaDosPequenos tabelaDosPrecos fatias [] !menorKey)) (valorDaLista (listaDosGrandes tabelaDosPrecos fatias [] !maiorKey !menorKey )) in
 print_string("YO2\n"); 
 print_int(minimo);
 let listaDeLucros = main fatias tabelaDosPrecos !maiorKey !maiorKey !menorKey minimo [||] arrNumDeFatiasDeTamanho [] in
 let resultado : int = maiorLucro listaDeLucros in
 let () =
 print_int(resultado)  in
 print_newline()
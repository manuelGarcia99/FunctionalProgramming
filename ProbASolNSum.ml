(**[((2n + 1)M(n-1)) + ((3n - 3)(M(n-2)))]/(n+2) *)
(**Formula M 29 = 593742784829 *)

open Z
let savings = Array.make 10001 (Z.of_int (0))

let two = Z.of_int 2
let three = Z.of_int 3

let rec motzkin input = 
  if(input=zero || input= one) then one 
  else if (input =two) then two
else if savings.(Z.to_int input) <> Z.of_int (0) then savings.(Z.to_int input)
else
  let output =
    (((two * input + one ) * motzkin (input - one)) + ((three * input - three) 
       * motzkin (input -two)))/(input + two) in
  let () = savings.(Z.to_int input) <- output in
  output;;

try
  let input_str = read_line () in
  let input = int_of_string input_str in
  let inputz = Z.of_int input in
  let resultado = Z.to_string (motzkin inputz) in
  Printf.printf"%s\n" resultado
with
| exn -> print_endline "You need to use a positive integer or zero as input!"
 

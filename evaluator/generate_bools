fun generate_booleans current_value =
    case current_value of 0 => []
|   _ => generate_booleans (current_value div 2)@[((current_value mod 2) = 1)]
;


fun value_list current_value =
    case current_value of 0 => [generate_booleans current_value]
|   _ => value_list (current_value - 1) @ [generate_booleans current_value]
;

fun long nil = 0
|   long (x::xs) = 1 + long xs
;   

fun quita_primero [] = []
|   quita_primero (x::xs) = xs
;

fun pow base exponent =
  if exponent = 0 then 1
  else
     base * pow base (exponent - 1)
;

fun agrega_falses (lista, num) = 
    case (lista, num) of (lista, 0) => lista
  | ((x::xs), n) => agrega_falses ((false::x::xs), num-1)
  | ([], n) => agrega_falses ((false::[]), num-1)
;


fun agrega (lista, numVar) =
  case (lista, numVar) of ([], n) => []
| (x::xs, n) => 
  if long x = 0 then []::agrega (xs, n)
  else
    if long x = n then x::agrega (xs, n)
  else
    if long x < n then (agrega_falses (x, (numVar - long x)))::agrega (xs, n)
  else (quita_primero x)::agrega (xs, n)
;


fun gen_bools numVar = 
    let
        val combinations = pow 2 numVar
        val lista =  value_list combinations
        val result = quita_primero lista (*Para quitar el [] del inicio*)
        val result = agrega (result, numVar)
        
    in
        result
    end
;
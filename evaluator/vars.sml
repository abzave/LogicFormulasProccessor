fun already_exists(elem, list) =
    case list of 
    [] => false 
    | (head::list) => if elem = head then true else already_exists(elem, list)
;
  
fun proposition_to_list proposition list = 
    case proposition of 
    negacion neg => list @ (proposition_to_list neg list)
    | conjuncion (conj1, conj2) =>  list @ (proposition_to_list conj1 list) @ (proposition_to_list conj2 list)
    | disyuncion  (dis1, dis2) =>  list @ (proposition_to_list dis1 list) @ (proposition_to_list dis2 list)
    | implicacion  (imp1, imp2) => list @ (proposition_to_list imp1 list) @ (proposition_to_list imp2 list)
    | equivalencia (equi1, equi2)=> list @ (proposition_to_list equi1 list) @ (proposition_to_list equi2 list)
    | variable var => [var]
    | constante const => [] 
;

fun eliminate_repeated list1 list2 =
    case list1 of 
    [] => list2
    | (head::rest) => if already_exists (head, list2) then eliminate_repeated rest list2 else eliminate_repeated rest (list2 @ [head])
;

fun vars proposition =
    eliminate_repeated (proposition_to_list(proposition) []) []
;



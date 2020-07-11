fun is_inverse proposition1 proposition2 =
    case proposition2 of
        negacion proposition3 
            => proposition3 = proposition1 
    | _ => false
;

fun simpl_aux proposition =
    case proposition of 
      conjuncion (prop1, prop2) 
        => if prop1 = prop2 
                then prop1 
           else 
                if is_inverse prop1 prop2 
                    then constante false
                else 
                    conjuncion (simpl_aux prop1, simpl_aux prop2)
    | disyuncion (prop1, prop2)
        => disyuncion (simpl_aux prop1, simpl_aux prop2)
    | implicacion (prop1, prop2)
        => if prop1 = prop2 
                then constante true 
           else if prop2 = constante true 
                then constante true
           else if prop1 = constante true 
                then prop2 
           else 
                implicacion (simpl_aux prop1, simpl_aux prop2)
    | negacion prop1
        =>  simpl_aux prop1
    | negacion (negacion prop1)
        => prop1
    | equivalencia (prop1, prop2)
        => if prop1 = prop2
                then constante true 
           else
                equivalencia (simpl_aux prop1, simpl_aux prop2)
    | variable prop1 
        => variable prop1
    | constante prop1  
        => constante prop1
;
  
fun simpl proposition =
    let
        val simplification = simpl_aux proposition
    in
        if simplification = proposition 
            then simplification
        else 
            simpl simplification
    end
;




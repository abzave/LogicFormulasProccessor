fun search_variable list var =
    case list of
    ((string , bool)::[]) => bool
    | ((string, bool)::rest) => if string = var then bool else search_variable rest var
;

fun evalProp assignment proposition =
    case proposition of
      constante valor
        => valor
    | variable var
        => search_variable assignment var
    | negacion prop1
        => not (evalProp assignment prop1)
    | conjuncion (prop1, prop2)
        => let val valor1 = evalProp assignment prop1
               and valor2 = evalProp assignment prop2
           in  valor1 andalso valor2
           end
    | disyuncion (prop1, prop2)
        => let val valor1 = evalProp assignment prop1
               and valor2 = evalProp assignment prop2
           in  valor1 orelse valor2
           end
    | implicacion (prop1, prop2)
        => let val valor1 = evalProp assignment prop1
               and valor2 = evalProp assignment prop2
           in  case (valor1, valor2) of
               (true, false) => false
               | _ => true
           end
    | equivalencia (prop1, prop2)
        => let val valor1 = evalProp assignment prop1
               and valor2 = evalProp assignment prop2
           in  valor1 = valor2
           end
;




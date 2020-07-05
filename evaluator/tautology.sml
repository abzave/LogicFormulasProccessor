fun evaluate_combinations combinations propostion = 
    case combinations of (combination::[]) => (evalProp combination proposition)::[]
    | (combination::combinations) => (evalProp combination proposition)::(evaluate_combinations combinations)
;

fun get_index_of_false list start_index = 
    case list of (head::[]) => if head then ~1 else start_index
    | (head::list) => if head then get_index_of_false list (start_index + 1) else start_index
    | [] => ~1
;

fun get_element_at list start_index end_index = 
    case list of (head::[]) => head
    | (head::list) => if start_index = end_index then head else get_element_at list (start_index + 1) end_index
;

fun bool_to_string bool = if bool then "true" else "false";

fun values_to_string assigments = 
    case assigments of ((logical_variable, value)::[]) => (logical_variable) ^ " = " ^ (bool_to_string value)
    | ((logical_variable, value)::assigments) => (logical_variable) ^ " = " ^ (bool_to_string value) ^ ", " ^(values_to_string assigments)
;

fun taut proposition = 
    let
        val simplification = simpl proposition
        val variables = vars simplification
        val values = gen_bools variables
        val evalation_results = evaluate_combinations values simplification
        val index_of_false = get_index_of_false evalation_results 0
    in
        if index_of_false = ~1 then
            print "Sí es tautología"
        else
            print "No es tautología, porque " ^ (values_to_string values) ^ " causa false"
    end
;
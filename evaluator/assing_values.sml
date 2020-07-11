fun long nil = 0
  | long (x::xs) = 1 + long xs
;

fun as_vals (valprops:string list, bools:bool list) = 
    case (valprops, bools) of ([], []) => [] 
  | (x::xs, []) => []
  | ([], y::ys) => []
  | (x::xs, y::ys) => 
    if (long valprops - long bools) = 0 
      then [(x,y)] @ as_vals (xs, ys)
    else []
;

fun as_vals_for_all (valprops:string list, bools:bool list list) = 
  case bools of [] => []
  | (asignment::bools) => (as_vals (valprops, asignment))::(as_vals_for_all (valprops, bools))
;
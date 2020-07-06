fun long nil = 0
  | long (x::xs) = 1 + long xs
;

fun as_vals (valprops:string list, bools: bool list) = 
    case (valprops, bools) of (nil,nil) => nil 
  | (x::xs, nil) => nil
  | (nil, y::ys) => nil
  | (x::xs, y::ys) => 
    if (long valprops - long bools) = 0 
      then [(x,y)] @ as_vals (xs, ys)
    else nil
;
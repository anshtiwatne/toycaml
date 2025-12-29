type 'a env = string -> 'a

exception Unbound of string

let empty x = raise (Unbound x)
let update env x v y = if y = x then v else env y

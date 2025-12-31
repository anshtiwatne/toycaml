open Error

type 'a env = string -> 'a

let empty x = raise (RuntimeError ("identifier '" ^ x ^ "' is not bound"))
let update env x v y = if y = x then v else env y

exception SyntaxError of string
exception TypeError of string
exception RuntimeError of string

let string_of_exception = function
  | SyntaxError msg -> "SyntaxError: " ^ msg
  | TypeError msg -> "TypeError: " ^ msg
  | RuntimeError msg -> "RuntimeError: " ^ msg
  | e -> Printexc.to_string e

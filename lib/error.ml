exception SyntaxError of string
exception TypeError of string
exception RuntimeError of string

let string_of_exception = function
  | SyntaxError msg -> "Syntax error: " ^ msg
  | TypeError msg -> "Type error: " ^ msg
  | RuntimeError msg -> "Runtime error: " ^ msg
  | e -> Printexc.to_string e

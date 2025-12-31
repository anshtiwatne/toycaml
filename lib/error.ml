exception SyntaxError of string
exception TypeError of string
exception RuntimeError of string

let string_of_exception = function
  | SyntaxError msg -> "Syntax Error: " ^ msg
  | TypeError msg -> "Type Error: " ^ msg
  | RuntimeError msg -> "Runtime Error: " ^ msg
  | e -> Printexc.to_string e

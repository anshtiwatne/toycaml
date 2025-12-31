open Ast

type token =
  (* Delimiters *)
  | LP
  | RP
  | COMMA
  | COLON
  (* Type Constructors *)
  | TINT
  | TBOOL
  | ARROW
  (* Unary Operators *)
  | MINUS (* Used for both subtraction and negation *)
  | NOT
  (* Binary Operators: Arithmetic *)
  | PLUS
  | TIMES
  (* Binary Operators: Comparison *)
  | EQ
  | NEQ
  | LT
  | GT
  (* Binary Operators: Logical *)
  | AND
  | OR
  (* Control Flow *)
  | IF
  | THEN
  | ELSE
  (* Pairs *)
  | FST
  | SND
  (* Functions *)
  | FUN
  | RFUN
  (* Bindings *)
  | LET
  | REC
  | IN
  (* Atoms *)
  | CONST of const
  | ID of string

let string_of_token = function
  | CONST (ICon i) -> string_of_int i
  | CONST (BCon b) -> string_of_bool b
  | ID s -> s
  | TINT -> "int"
  | TBOOL -> "bool"
  | IF -> "if"
  | THEN -> "then"
  | ELSE -> "else"
  | LET -> "let"
  | REC -> "rec"
  | IN -> "in"
  | FUN -> "fun"
  | RFUN -> "rfun"
  | FST -> "fst"
  | SND -> "snd"
  | LP -> "("
  | RP -> ")"
  | COMMA -> ","
  | COLON -> ":"
  | ARROW -> "->"
  | PLUS -> "+"
  | MINUS -> "-"
  | TIMES -> "*"
  | EQ -> "="
  | NEQ -> "<>"
  | LT -> "<"
  | GT -> ">"
  | AND -> "&&"
  | OR -> "||"
  | NOT -> "!"

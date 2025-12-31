open Ast
open Token
open Dynamic

let rec string_of_ty = function
  | TInt -> "int"
  | TBool -> "bool"
  | TArrow (t1, t2) -> "(" ^ string_of_ty t1 ^ " -> " ^ string_of_ty t2 ^ ")"
  | TPair (t1, t2) -> "(" ^ string_of_ty t1 ^ " * " ^ string_of_ty t2 ^ ")"

let rec string_of_val = function
  | IV i -> string_of_int i
  | BV b -> string_of_bool b
  | PairV (v1, v2) -> "(" ^ string_of_val v1 ^ ", " ^ string_of_val v2 ^ ")"
  | FunV _ -> "<fun>"
  | RFunV _ -> "<rfun>"

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

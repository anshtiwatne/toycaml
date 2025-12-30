open Ast

type token =
  (* Delimiters *)
  | LP
  | RP
  | COMMA
  | COL
  (* Type Constructors *)
  | INT
  | BOOL
  | ARR
  (* Unary Operators *)
  | SUB (* Used for both subtraction and negation *)
  | NOT
  (* Binary Operators: Arithmetic *)
  | ADD
  | MUL
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
  | CON of const
  | VAR of string

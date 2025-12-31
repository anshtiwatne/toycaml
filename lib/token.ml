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

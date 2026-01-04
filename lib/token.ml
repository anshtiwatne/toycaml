open Ast

type token =
  (* Delimiters *)
  | LPAREN
  | RPAREN
  | COMMA
  | COLON
  | LBRACK
  | RBRACK
  | SEMICOLON
  (* Type Constructors *)
  | TINT
  | TBOOL
  | ARROW
  (* List Operators *)
  | CONS
  | APPEND
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

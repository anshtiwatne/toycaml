type ty = Bool | Int | Arrow of ty * ty | Product of ty * ty
type con = BCon of bool | ICon of int
type var = string
type op = Add | Sub | Mul | Leq

type exp =
  | Con of con
  | Var of var
  | If of exp * exp * exp
  | OApp of op * exp * exp
  | Fun of var * ty * exp
  | RFun of var * var * ty * ty * exp
  | FApp of exp * exp
  | Pair of exp * exp
  | Fst of exp
  | Snd of exp

(* type token =
  | LP
  | RP
  | COL
  | ARR
  | COMMA
  | ADD
  | SUB
  | MUL
  | LEQ
  | IF
  | THEN
  | ELSE
  | FUN
  | BOOL
  | INT
  | CON of con
  | VAR of string *)

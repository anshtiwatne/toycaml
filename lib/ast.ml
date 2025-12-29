type ty = Bool | Int | Arrow of ty * ty | Product of ty * ty
type con = BCon of bool | ICon of int
type op = Add | Sub | Mul | Leq
type var = string

type exp =
  | Var of var
  | Con of con
  | OApp of op * exp * exp
  | FApp of exp * exp
  | If of exp * exp * exp
  | Fun of var * ty * exp
  | Pair of exp * exp
  | Fst of exp
  | Snd of exp
  | RFun of var * var * ty * ty * exp

type token =
  | LP
  | RP
  | COL
  | ARR
  | ADD
  | SUB
  | MUL
  | LEQ
  | IF
  | THEN
  | ELSE
  | FUN
  | CON of con
  | VAR of string
  | BOOL
  | INT
  | COMMA

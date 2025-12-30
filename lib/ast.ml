type id = string
type ty = TInt | TBool | TArrow of ty * ty | TPair of ty * ty
type unop = Neg | Not

type binop =
  (* Arithmetic *)
  | Add
  | Sub
  | Mul
  (* Comparison *)
  | Eq
  | Neq
  | Lt
  | Gt
  | Leq
  (* Logical (Lazy) *)
  | And
  | Or

type const = ICon of int | BCon of bool

type exp =
  (* Atoms *)
  | Var of id
  | Const of const
  (* Operations *)
  | UnOp of unop * exp
  | BinOp of binop * exp * exp
  (* Control Flow *)
  | If of exp * exp * exp
  (* Pairs *)
  | Pair of exp * exp
  | Fst of exp
  | Snd of exp
  (* Functions *)
  | App of exp * exp
  | Fun of id * ty * exp
  | RFun of id * id * ty * ty * exp
  (* Bindings *)
  | Let of id * exp * exp
  | LetRec of id * id * ty * ty * exp * exp

(* Slight design inconsistency:
RFun/LetRec require some types that Fun/Let do not and can infer.
I could skip inference and require types everywhere but that would make elaboration less interesting.
Maybe eventually I'll work on implementing type inference for RFun/LetRec also *)

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

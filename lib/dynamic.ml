open Ast
open Env
open Error

type value =
  | IV of int
  | BV of bool
  | PairV of value * value
  | FunV of id * exp * value env
  | RFunV of id * id * exp * value env

let rec eval env = function
  (* Atoms *)
  | Const (ICon i) -> IV i
  | Const (BCon b) -> BV b
  | Var x -> env x
  (* Operations *)
  | UnOp (op, e) -> (
      let v = eval env e in
      match (op, v) with
      | Neg, IV i -> IV (-i)
      | Not, BV b -> BV (not b)
      | _ -> raise (RuntimeError "Unary operator type mismatch"))
  | BinOp (op, e1, e2) -> (
      (* Lazy Logic *)
      match op with
      | And -> (
          match eval env e1 with
          | BV false -> BV false
          | BV true -> eval env e2
          | _ -> raise (RuntimeError "And requires bool"))
      | Or -> (
          match eval env e1 with
          | BV true -> BV true
          | BV false -> eval env e2
          | _ -> raise (RuntimeError "Or requires bool"))
      | _ -> (
          let v1 = eval env e1 in
          let v2 = eval env e2 in
          match (op, v1, v2) with
          (* Arithmetic *)
          | Add, IV i1, IV i2 -> IV (i1 + i2)
          | Sub, IV i1, IV i2 -> IV (i1 - i2)
          | Mul, IV i1, IV i2 -> IV (i1 * i2)
          (* Comparison *)
          | Lt, IV i1, IV i2 -> BV (i1 < i2)
          | Gt, IV i1, IV i2 -> BV (i1 > i2)
          | Eq, _, _ -> BV (v1 = v2)
          | Neq, _, _ -> BV (v1 <> v2)
          | _ -> raise (RuntimeError "Binary operator type mismatch")))
  (* Control Flow *)
  | If (e1, e2, e3) -> (
      match eval env e1 with
      | BV true -> eval env e2
      | BV false -> eval env e3
      | _ -> raise (RuntimeError "If condition must be Bool"))
  (* Pairs *)
  | Pair (e1, e2) -> PairV (eval env e1, eval env e2)
  | Fst e -> (
      match eval env e with
      | PairV (v1, _) -> v1
      | _ -> raise (RuntimeError "Fst requires a pair"))
  | Snd e -> (
      match eval env e with
      | PairV (_, v2) -> v2
      | _ -> raise (RuntimeError "Snd requires a pair"))
  (* Functions *)
  | Fun (x, _, e) -> FunV (x, e, env)
  | RFun (f, x, _, _, e) -> RFunV (f, x, e, env)
  | App (e1, e2) -> (
      let v_fun = eval env e1 in
      let v_arg = eval env e2 in
      match v_fun with
      | FunV (x, body, env_inner) -> eval (update env_inner x v_arg) body
      | RFunV (f, x, body, env_inner) ->
          let env_rec = update env_inner f v_fun in
          eval (update env_rec x v_arg) body
      | _ -> raise (RuntimeError "Cannot apply non-function"))
  (* Bindings *)
  | Let (x, e1, e2) ->
      let v1 = eval env e1 in
      eval (update env x v1) e2
  | LetRec (f, x, _, _, e1, e2) ->
      let closure = RFunV (f, x, e1, env) in
      eval (update env f closure) e2

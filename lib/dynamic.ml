open Ast
open Env

type value =
  | IV of int
  | FunV of var * exp * value env
  | RFunV of var * var * exp * value env

let rec eval env = function
  | Con (BCon b) -> IV (if b then 1 else 0)
  | Con (ICon i) -> IV i
  | Var x -> env x
  | OApp (op, e1, e2) -> (
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      match (op, v1, v2) with
      | Add, IV i1, IV i2 -> IV (i1 + i2)
      | Sub, IV i1, IV i2 -> IV (i1 - i2)
      | Mul, IV i1, IV i2 -> IV (i1 * i2)
      | Leq, IV i1, IV i2 -> IV (if i1 <= i2 then 1 else 0)
      | _ -> failwith "Runtime operator error")
  | If (e1, e2, e3) -> (
      match eval env e1 with
      | IV 0 -> eval env e3
      | IV _ -> eval env e2
      | _ -> failwith "Runtime if error")
  | Fun (x, _, e) -> FunV (x, e, env)
  | RFun (f, x, _, _, e) -> RFunV (f, x, e, env)
  | FApp (e1, e2) -> (
      let v_arg = eval env e2 in
      match eval env e1 with
      | FunV (x, body, closure_env) -> eval (update closure_env x v_arg) body
      | RFunV (f, x, body, closure_env) ->
          let env' = update closure_env f (RFunV (f, x, body, closure_env)) in
          eval (update env' x v_arg) body
      | _ -> failwith "Runtime application error")
  | Pair (e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      FunV
        ( "fst",
          Fun ("snd", Int, Con (ICon 0)),
          update (update empty "fst" v1) "snd" v2 )
  | Fst e -> (
      match eval env e with
      | FunV ("fst", _, closure_env) -> closure_env "fst"
      | _ -> failwith "Runtime fst error")
  | Snd e -> (
      match eval env e with
      | FunV ("snd", _, closure_env) -> closure_env "snd"
      | _ -> failwith "Runtime snd error")
